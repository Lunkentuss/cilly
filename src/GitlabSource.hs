module GitlabSource (
  makeSource
) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack, replace)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Data.Text hiding (elem, filter, map, findIndex, all, concatMap)
import Data.List hiding (map, all, isPrefixOf)
import Data.Aeson.KeyMap (toList)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import Data.Aeson.Key (toString)
import Data.Yaml (
    encode
  , decode
  , FromJSON
  , ToJSON
  , parseJSON
  , withObject
  , Object
  , ParseException
  , decodeEither
  , (.:)
  , (.:?)
  , (.!=)
  )
import qualified Data.Yaml as Yaml
import Data.Aeson.Types (Key, Parser, Value(..))
import GHC.Generics (Generic)
import System.Exit

import qualified Source

-- The yaml library doesn't support yaml tags since its parse results are Aeson
-- values. This string is used in a preprocess stage that substitutes tags and
-- injects this arbitrary string into the first element of the array, which is
-- then used to check if an array is a tagged reference or not.
referenceSubstituteString :: Text
referenceSubstituteString = "__ReFeReNcE_SuBsTiTuTe_StrinGG__"

isUnavailableJobName :: String -> Bool
isUnavailableJobName entry = entry `elem` unavailableJobNames where
  unavailableJobNames = [
    "image",
    "services",
    "stages",
    "types",
    "before_script",
    "after_script",
    "variables",
    "cache",
    "include"]

isValidJobName :: String -> Bool
isValidJobName entry = all ($ entry) [isValidName, not . isUnavailableJobName]
  where isValidName (x:_) = x /= '.'
        isValidName "" = False

defaultStage :: String
defaultStage = "test"

defaultStages :: [String]
defaultStages = ["build", "test", "deploy"]

makeSource :: Source.Source
makeSource = Source.Source {
  name = "Gitlab",
  isValidPipeline = isValidPipeline,
  makePipeline = makePipeline
  }

makePipeline :: Source.Path -> IO Source.Pipeline
makePipeline root = do
  gitlabCiPreprocessed <-
      TE.encodeUtf8
      . replace "!reference [" ("[" <> referenceSubstituteString <> ",")
      <$> TIO.readFile ".gitlab-ci.yml"
  object <- case decodeEither gitlabCiPreprocessed of
    Right pipeline -> return pipeline
    Left err -> exit $ show err
  return $ makePipelineNoIO object

-- Checks if a gitlab source is available.
-- Return always for now.
isValidPipeline :: Source.Path -> IO Bool
isValidPipeline root = return True

makePipelineNoIO Pipeline {..} =
  let
    allStages = stages
      & (if ".pre" `elem` stages then id else ([".pre"] <>))
      & (if ".post" `elem` stages then id else (<> [".post"]))
  in
    Source.Pipeline {
        jobs = makeJobs allStages variables jobs
      , stages = allStages
    }

makeJobs stages variables jobs_ =
  let nonEmptyStages = stages
        & filter (`elem` map stage jobs_)
      stageToJobs = nonEmptyStages
        & map (\stage_ -> (stage_, filter ((==stage_) . stage) jobs_))
        & Map.fromList
      prevNonEmptyStage stage_ = nonEmptyStages
        & elemIndex stage_
        & (subtract 1 <$>)
        & (>>= (\x -> if x < 0 then Nothing else Just x))
        & ((nonEmptyStages!!)<$>)
      makeJob_ job = makeJob
        job
        (stage job
          & prevNonEmptyStage
          & maybe [] (stageToJobs Map.!)
          & map name
        )
        variables
  in map makeJob_ jobs_

makeJob (Job {..}) jobDependencies pipelineVariables = Source.Job {
    artifacts = makeArtifacts artifacts
  , jobDependencies = jobDependencies
  , scripts = [
        Source.Script (beforeScript <> script) allowFailure
    ] <> maybe [] (\script -> [Source.Script script True]) afterScript
  , variables = variables <> pipelineVariables
  , ..
  }
makeArtifacts Artifacts {..} = Source.Artifacts {..}

parseKeyMap
  :: Yaml.Object
  -> (Key -> Bool)
  -> (Key -> Value -> Parser a)
  -> Parser [a]
parseKeyMap object filterf parser =
  toList object
    & filter (\(key, _) -> filterf key)
    & traverse (uncurry parser)

-- TODO: Fix
-- substituteReferences :: Yaml.Object -> Either String Yaml.Object
substituteReferences :: Yaml.Object -> Yaml.Object
substituteReferences pipeline = mapObject pipeline
  where
    resolveRef :: Value -> [String] -> Maybe Value
    resolveRef value refList = case refList of
      (key:refList') -> case value of
        Object object -> case KM.lookup (Key.fromText . pack $ key) object of
          Just childValue -> resolveRef childValue refList'
          Nothing -> Nothing
        _ -> Nothing
      _ -> Just value

    mapObject :: Yaml.Object -> Yaml.Object
    mapObject = KM.map (fst . go)

    isReferenceString :: Value -> Bool
    isReferenceString value = case value of
      String text -> text == referenceSubstituteString
      _ -> False

    filterArrayStrings :: [Value] -> [String]
    filterArrayStrings = concatMap filterString
      where
        filterString :: Value -> [String]
        filterString v = case v of
          String str -> [unpack str]
          _ -> []

    go :: Value -> (Value, Bool)
    go value = case value of
      Object object -> (Object $ mapObject object, False)
      Array array -> case Vector.toList array of
        (x:xs) -> case x of
          x | isReferenceString x -> case resolveRef (Object pipeline) (filterArrayStrings xs) of
              Just value -> (value, True)
              _ -> (String "", False) -- TODO: Fix
          _ -> goArray $ x : xs
        list -> goArray list
      v -> (v, False)
      where
        goArray list = (Array $ Vector.fromList $ concatMap (mapArrayValue . go) list, False)
        mapArrayValue :: (Value, Bool) -> [Value]
        mapArrayValue valueTuple = case valueTuple of
          (value, True) -> case value of
            Array array -> Vector.toList array
            v -> [v]
          (value, False) -> [value]

data Pipeline = Pipeline {
  stages :: [String],
  jobs :: [Job],
  variables :: Map.Map String String
} deriving (Show)

data Job = Job {
    name :: String
  , image :: Maybe String
  , entrypoint :: Maybe [String]
  , stage :: String
  , artifacts :: Artifacts
  , beforeScript :: [String]
  , script :: [String]
  , afterScript :: Maybe [String]
  , allowFailure :: Bool
  , variables :: Map.Map String String
} deriving (Show)

newtype Artifacts = Artifacts {
  paths :: [String]
} deriving (Show, Generic)

instance FromJSON Artifacts

instance FromJSON Pipeline where
  parseJSON = withObject "Pipeline" $ \v' ->
    let
      v = substituteReferences v'
    in
      Pipeline
        <$> v .:? "stages" .!= defaultStages
        <*> parseKeyMap v (isValidJobName . toString) (parseJob . toString)
        <*> v .:? "variables" .!= Map.empty

parseJob :: String -> Value -> Parser Job
parseJob name = withObject "Job" $ \o -> do
  imageValue <- o .:? "image"
  (image, entrypoint) <- case imageValue of
    Just imageValue' -> case imageValue' of
      Yaml.Object imageObject -> do
        image <- imageObject .:? "name"
        entrypoint <- imageObject .:? "entrypoint"
        return (image, entrypoint)
      Yaml.String image -> return (Just $ unpack image, Nothing)
      _ -> fail "Invalid image input type"
    Nothing -> return (Nothing, Nothing)
  stage <- o .:? "stage" .!= defaultStage
  artifacts <- o .:? "artifacts" .!= Artifacts []
  beforeScript <- o .:? "before_script" .!= []
  script <- o .: "script"
  afterScript <- o .:? "after_script" .!= Nothing
  allowFailure <- o .:? "allow_failure" .!= False
  variables <- o .:? "variables" .!= Map.empty
  return $ Job {name = name, ..}

exit err = do
  putStrLn err
  exitWith (ExitFailure 1)
