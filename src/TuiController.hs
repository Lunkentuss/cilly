module TuiController(
  controller
) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Focus as Focus
import Brick.Types (get)
import Brick.Widgets.Border as Border
import Brick.Widgets.Center as Center
import Brick.Widgets.List
import qualified Brick.Widgets.ProgressBar as ProgressBar
import Control.Concurrent
import Control.Lens hiding (zoom)
import Control.Monad
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Map.Lens
import qualified Data.Vector as Vector
import Data.List as List
import Data.List.Split
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty ((<|>))
import Data.Maybe
import System.Exit (ExitCode(..))
import Text.Read (readMaybe)

import Concurrent
import JobDispatcher
import qualified Source

type JobList = GenericList ControllerName Vector.Vector String

data ControllerName = JobListName | LogName deriving (Eq, Ord, Show)

data ControllerEvent = JobMessage DispatcherJobMessage | Tick

data JobStatus = NotStarted | Running | Finished | Failed deriving (Show, Eq)

data AttrSegment = AttrSegment
  {
    attr :: Vty.Attr
  , text :: String
  } deriving Show

type AttrSegments = [AttrSegment]
type AnsiDoc = [AttrSegments]

data LogAction = LogUp | LogDown | LogTop | LogBottom
type LogActions = [LogAction]

data JobState = JobState
  {
    _logs :: AnsiDoc
  , _userLogActions :: LogActions
  , _status :: JobStatus
  }
$(makeFieldsNoPrefix ''JobState)

data ControllerState = ControllerState
  {
    _jobToState :: Map.Map String JobState
  , _pipeline :: Source.Pipeline
  , _jobList :: JobList
  , _loaderSymbols :: [String]
  , _focusRing :: Focus.FocusRing ControllerName
  }
$(makeFieldsNoPrefix ''ControllerState)

brickChannelSize :: Int
brickChannelSize = 100

-- Loader symbol
loaderSymbolsList = ["|", "/", "-", "\\"]
tickDelay = 100000

-- UI
jobListWidthPercentage = 30

-- Theme and attributes
selectJobAttr = attrName "selectJob"
focusAttr = attrName "focus"
noFocusAttr = attrName "noFocus"

attributeMap :: AttrMap
attributeMap = attrMap Vty.defAttr
  [
      (selectJobAttr, fg Vty.blue)
    , (focusAttr, bg Vty.blue)
    , (noFocusAttr, Vty.defAttr)
    , (ProgressBar.progressCompleteAttr, bg Vty.blue)
  ]

safeTail :: [a] -> [a] -> [a]
safeTail d [] = d
safeTail d (x:xs) = xs

onHead :: (a -> a) -> [a] -> [a]
onHead _ [] = []
onHead f (x:xs) = f x : xs

drawUI :: ControllerState -> [Widget ControllerName]
drawUI ControllerState {..} =
  let
    jobToStatus jobId = _status $ _jobToState Map.! jobId
    loaderSymbol = head _loaderSymbols
    focusBorder name label widget =
      let isFocused = Just name == Focus.focusGetCurrent _focusRing
          focusedAttr = if isFocused then focusAttr else noFocusAttr
      in Border.borderWithLabel (withAttr focusedAttr $ str label) widget
    selectedJob = _jobToState Map.! fromMaybe "SET_TODO" (selectedElement _jobList)
    statusToString status =
      let
        loader = if status == Running then loaderSymbol else ""
      in
        (<> loader)
        $ (("["<>) . (<>"]"))
        $ show status
    progress =
      (/)
      (
      Map.elems _jobToState
      & filter ((==Finished) . _status)
      & length
      & fromIntegral
      )
      (Map.size _jobToState & fromIntegral)
  in
    return $
    vBox
    [
        hBox
        [
          focusBorder JobListName "Jobs"
          $ hLimitPercent jobListWidthPercentage
          $ renderList
            (\isSelected job ->
                let
                  jobWithStatus =
                    job
                    <> " "
                    <> statusToString (jobToStatus job)
                in
                  if isSelected
                    then withAttr selectJobAttr $ str jobWithStatus
                    else str jobWithStatus
            )
            True
            _jobList
        ,
          focusBorder LogName "Log"
          $ ansiWidget
              (selectedJob ^. userLogActions)
              (selectedJob ^. logs)
          -- TODO: Fix default value if unselected
        ]
      , Border.border
        $ ProgressBar.progressBar
            Nothing
            progress
    ]

selectedElement :: JobList -> Maybe String
selectedElement jobList = snd <$> listSelectedElement jobList

app :: App ControllerState ControllerEvent ControllerName
app = App
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const attributeMap
  }

handleEvent
  :: BrickEvent ControllerName ControllerEvent
  -> EventM ControllerName ControllerState ()
handleEvent (AppEvent (JobMessage msg)) = do
  let updateJob jobId state f = state & jobToState . ix jobId %~ f
  let updateStatus newStatus jobId state =
        updateJob jobId state (status .~ newStatus)
  case msg of
        DJobFinished {..} ->
          modify $ updateStatus Finished jobId
        DJobOutput {..} ->
          modify $ \s -> updateJob jobId s (logs %~ appendAnsiDoc message)
        DJobsStarted {..} ->
          modify $ \s -> foldr (updateStatus Running) s jobIds
        DJobFailed {..} ->
          modify $ updateStatus Failed jobId
        DAllJobsFinished -> return ()
handleEvent (AppEvent Tick) = do
  modify $ loaderSymbols %~ tail

handleEvent (VtyEvent (Vty.EvKey Vty.KEsc [])) = halt
handleEvent (VtyEvent (Vty.EvKey Vty.KRight [])) =
  modify $ focusRing %~ Focus.focusNext
handleEvent (VtyEvent (Vty.EvKey Vty.KLeft [])) =
  modify $ focusRing %~ Focus.focusPrev
handleEvent (VtyEvent event) = do
  prevState <- get
  let focusedName = prevState ^. focusRing & Focus.focusGetCurrent
  case focusedName of
      Just JobListName -> do
        zoom jobList $ handleListEvent event
      Just LogName -> do
        case selectedElement $ prevState ^. jobList of
          Just selectedJob -> do
            let addAction = case event of
                  (Vty.EvKey Vty.KUp []) -> (<> [LogUp])
                  (Vty.EvKey Vty.KDown []) -> (<> [LogDown])
                  (Vty.EvKey (Vty.KChar 'g') []) -> (<> [LogTop])
                  (Vty.EvKey (Vty.KChar 'G') []) -> (<> [LogBottom])
                  _ -> id
            modify $ jobToState . ix selectedJob . userLogActions %~ addAction
          _ -> return ()
      _ -> return ()
handleEvent _ = return ()

-- ============ Ansi parsing

esc = "\ESC["
escEnd = 'm'
escCodeDelimiter = ";"
tabLength = 4

data AnsiSegment = AnsiSegment
  {
    escapeCodes :: [Int]
  , text :: String
  } deriving Show

-- Parses the beginning of an ANSI sequence, i.e., everything after the escape
-- character, e.g., legit input can be "[31;1mMy String\n". The input must not
-- include any escape characters.
parseSegment :: String -> AnsiSegment
parseSegment input =
  let codes = takeWhile (/= escEnd) input
      rest = safeTail "" $ dropWhile (/= escEnd) input
  in AnsiSegment {
    escapeCodes =
      fromMaybe [] $
        (mapM readMaybe . splitOn escCodeDelimiter)
        codes
  , text = rest
  }

tabToSpaces :: String -> String
tabToSpaces =
  concatMap (\c -> if c == '\t' then replicate tabLength ' ' else [c])

parseAnsiSegments :: String -> [AnsiSegment]
parseAnsiSegments =
  map parseSegment
  . onHead fixHead
  . splitOn esc
  . tabToSpaces
  where fixHead string = if string == "" then string else escEnd : string

-- ============ Attr parsing

codeToAttrMap :: [(Int, Vty.Attr -> Vty.Attr)]
codeToAttrMap =
  [
    (0,  const Vty.defAttr)
  , (1,  (`Vty.withStyle` Vty.bold))
  , (30, (`Vty.withForeColor` Vty.black))
  , (31, (`Vty.withForeColor` Vty.red))
  , (32, (`Vty.withForeColor` Vty.green))
  , (33, (`Vty.withForeColor` Vty.yellow))
  , (34, (`Vty.withForeColor` Vty.blue))
  , (35, (`Vty.withForeColor` Vty.magenta))
  , (36, (`Vty.withForeColor` Vty.cyan))
  , (37, (`Vty.withForeColor` Vty.white))
  ]

codesToAttr :: [Int] -> Vty.Attr -> Vty.Attr
codesToAttr =
  foldr
    (flip (.) . fromMaybe id . (`lookup` codeToAttrMap))
    id

appendAnsiDoc :: String -> AnsiDoc -> AnsiDoc
appendAnsiDoc input = appendAnsiDocNoSanitize (filter (/='\r') input)

appendAnsiDocNoSanitize :: String -> AnsiDoc -> AnsiDoc
appendAnsiDocNoSanitize "" doc = doc
appendAnsiDocNoSanitize ('\n':rest) doc = appendAnsiDoc rest $ doc <> [[]]
appendAnsiDocNoSanitize input doc =
  let
    untilNewLine = takeWhile (/= '\n') input
    rest = dropWhile (/= '\n') input
    line = if null doc then [] else last doc
    initDoc = if null doc then [] else init doc
  in appendAnsiDoc
    rest
    (initDoc
     <> [appendAttrSegments untilNewLine (docAttr doc) line]
    )

-- Returns the current attribute for the document.
docAttr :: AnsiDoc -> Vty.Attr
docAttr doc =
  let rev = concat $ List.reverse $ map List.reverse doc
  in if null rev then Vty.defAttr else attr $ head rev

-- Returns the `lineCount` most recent lines. All lines
-- are wrapped by `wrapWidth`.
wrapLastAnsiDoc :: Int -> Int -> AnsiDoc -> AnsiDoc
wrapLastAnsiDoc lineCount wrapWidth =
  List.reverse
  . take lineCount
  . concatMap (List.reverse . wrapSegments wrapWidth)
  . List.reverse

appendAttrSegments :: String -> Vty.Attr -> AttrSegments -> AttrSegments
appendAttrSegments input baseSegmentsAttr prevSegments =
  let
    newSegments =
      fst
      $ foldl
        (
          \(attrSegments,prevAttr) segment ->
          let toAttr =
                codesToAttr (escapeCodes segment) . prevAttr
              newAttrSegment =
                AttrSegment {
                    attr = toAttr Vty.defAttr
                  , text = segment.text
                }
          in
          (attrSegments <> [newAttrSegment], toAttr)
        )
        ([], const baseSegmentsAttr)
      $ parseAnsiSegments input
  in prevSegments <> newSegments

wrapSegments :: Int -> AttrSegments -> [AttrSegments]
wrapSegments wrapWidth segments = go segments [[]] 0
  where
  go :: AttrSegments -> [AttrSegments] -> Int -> [AttrSegments]
  go currentSegments currentLines currentOffset =
    case currentSegments of
      (segment:nextSegments) ->
        let
          currentLine = last currentLines
          segmentText = segment.text
          nextOffset = mod (currentOffset + textWidth segmentText) wrapWidth
          splitText =
            splitPlaces
              ([wrapWidth - currentOffset] <> repeat wrapWidth)
              segmentText
          newSegments =
            map (\text' -> AttrSegment { attr = segment.attr, text = text'}) splitText
          nextLines =
            init currentLines
            <> [
                currentLine
                <> [head newSegments | not (null newSegments)]
               ]
            <> map pure (safeTail [] newSegments)
        in go nextSegments nextLines nextOffset
      _ -> currentLines

ansiWidget
  :: LogActions
  -> AnsiDoc
  -> Widget n
ansiWidget logActions doc =
  Widget Greedy Greedy $ do
    cxt <- getContext
    let (width, height) = (availWidth cxt, availHeight cxt)
    render $ raw $ ansiImage height width logActions doc

ansiImage
  :: Int
  -> Int
  -> LogActions
  -> AnsiDoc
  -> Vty.Image
ansiImage lineCount wrapWidth logActions doc =
  Vty.resize wrapWidth lineCount
    $ foldMap mkLine
    $ wrapLastAnsiDoc lineCount wrapWidth (iterate init doc !! offset)
  where
    mkLine =
      foldr
        (
          (Vty.<|>) .
          (\segment ->
            Vty.string
              (attr segment)
              (segment.text)
          )
        )
        mempty
    applyAction prevOffset action =
      let maxOffset = max 0 $ length doc - lineCount
          newOffset = case action of
            LogUp -> prevOffset + 1
            LogDown -> prevOffset - 1
            LogBottom -> 0
            LogTop -> maxOffset
          truncatedOffset
            | newOffset < 0 = 0
            | maxOffset <= newOffset = maxOffset
            | otherwise = newOffset
      in truncatedOffset
    offset = foldl applyAction 0 logActions

controller
  :: Source.Pipeline
  -> TMessages DispatcherJobMessage
  -> IO ExitCode
controller pipeline messages = do
  chan <- newBChan brickChannelSize
  forkIO $ forever $ do
    msg <- popIO messages
    writeBChan chan (JobMessage msg)
  forkIO $ forever $ do
    threadDelay tickDelay
    writeBChan chan Tick
  let buildVty = Vty.mkVty Vty.defaultConfig
  let jobNames = map (.name)
        $ Source.jobs pipeline
  initVty <- buildVty
  void $ customMain initVty buildVty (Just chan) app
    $ ControllerState {
          _jobToState = Map.fromList
              $ map
                  (,JobState
                    {
                      _status = NotStarted
                    , _userLogActions = []
                    , _logs = []
                    }
                  )
                  jobNames
        , _pipeline = pipeline
        , _jobList = list JobListName (Vector.fromList jobNames) 1
        , _loaderSymbols = cycle loaderSymbolsList
        , _focusRing = Focus.focusRing [JobListName, LogName]
      }
  return ExitSuccess
