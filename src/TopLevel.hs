module TopLevel where

import Game.Model.World hiding (Inventory)

import Tokenizer (tokens, Lexeme, runParser)
import System.Console.Haskeline
        (InputT, getInputLine, runInputT, defaultSettings)
import qualified Data.Char (toUpper)
import Recognizer (Imperative, imperative, parser, fullParses)
import Build.GameState (buildGameState)
import qualified Data.Text
import Game.Narration (displaySceneM)
import Game.Engine (catchEngine)
import Clarifier (doReportM, clearReportM)
import Game.Model.Display

data GameInput
    = MkGameInput Text
    | Verbose
    | Inventory
    | Quit

start :: GameStateExceptT ()
start = buildGameState >> topLevel

topLevel :: GameStateExceptT ()
topLevel = get >>= _displayAction' >> clearReportM >> inputAction

inputAction :: GameStateExceptT ()
inputAction = do
  input <- liftIO getInput
  case input of
    Nothing -> inputAction
    Just Verbose -> setVerbosityM >> topLevel
    Just Quit -> pass
    Just Inventory -> describeInventoryM 
                        >> updateDisplayActionM showEnvironmentM 
                        >> topLevel
    Just (MkGameInput input') -> do
      case runParser tokens input' of
          Left _ -> putStrLn "parse failed" >> displaySceneM True >> inputAction
          Right tokens' -> either catchBadParseM catchEngine (parseTokens tokens')
                            >> topLevel

catchBadParseM :: Text -> GameStateExceptT ()
catchBadParseM = doReportM

getInput :: IO (Maybe GameInput)
getInput = do
  str <- runInputT defaultSettings go
  case str of
    ""     -> pure Nothing
    "quit" -> pure $ Just Quit
    "inventory" -> pure $ Just Inventory
    "verbose" -> pure $ Just Verbose
    input  -> pure $ Just (mkGameInput input)
  where
    go :: InputT IO String
    go = do
          minput <- getInputLine "% "
          case minput of
            Nothing -> go
            Just "" -> go
            Just input -> pure input

mkGameInput :: String -> GameInput
mkGameInput str = MkGameInput (toText $ map Data.Char.toUpper str)

parseTokens :: [Lexeme] -> Either Text Imperative
parseTokens toks =
  case parsed of
    (parsed':_) -> Right parsed'
    _          -> Left (Data.Text.concat ("Nonsense in parsed tokens " : toks'))
    where
      parsed = fst $ fullParses (parser imperative) toks
      toks' = map toText toks