module Shell (
    ShellCommand(..)
  , ShellCommands
  , commandsToShellScript
  , quote
) where

import Data.Function
import Data.List
import qualified Data.Map as Map

-- ExecuteShell:     Execute a shell command, log stdout/stderr to terminal.
-- ExecuteShellQuit: Execute a shell command, don't log any output to terminal.
-- LogShell:         Dont execute anything, only echo the string.
data ShellCommand =
    ExecuteShell String
  | ExecuteShellQuiet String
  | LogShell String

type ShellCommands = [ShellCommand]

commandsToShellScript :: [ShellCommand] -> Map.Map String String -> String
commandsToShellScript commands variables =
  commands
  & map toShell
  & (variablesToShell variables <>)
  & intercalate ";"
  & quote
  & ("eval $"++)
  & (++"\nexit 0\n")
  & ("set +o noclobber\n"++)
  & ("set -eo pipefail\n"++)

toShell :: ShellCommand -> String
toShell command = case command of
  ExecuteShell command -> command & escape
  ExecuteShellQuiet command -> command & escape & (<> " &>/dev/null")
  LogShell log -> log & escapeInQuoteC & quoteC & greenify & ("echo "++)

variablesToShell :: Map.Map String String -> [String]
variablesToShell variables = variables
  & Map.toList
  & map (\(key, value) -> escape $  "export " <> key <> "=" <> value)

colorize :: Int -> String -> String
colorize code str =
  quoteAnsiC (
    "\\\\033["
    ++ show code
    ++ ";1m"
  )
  ++ str
  ++ quoteAnsiC "\\\\033[0m"

greenify :: String -> String
greenify = colorize 32
quote :: String -> String
quote str = "'" ++ str ++ "'"
quoteAnsiC :: String -> String
quoteAnsiC = ("$"++) . quoteC
quoteC :: String -> String
quoteC str = "\\x27" ++ str ++ "\\x27"
escape :: String -> String
escape = concatMap (\c -> if c == '\'' then "\\x27" else [c])
escapeInQuoteC :: String -> String
escapeInQuoteC = concatMap (\c -> if c == '\'' then "\\x27\"\\x27\"\\x27" else [c])
