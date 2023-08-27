module HauntedHouse.TopLevel where

import HauntedHouse.Game.Model.World
       
import HauntedHouse.Tokenizer (tokens, Lexeme, runParser)
import System.Console.Haskeline
        (InputT, getInputLine, runInputT, defaultSettings)
import qualified Data.Char (toUpper)
import HauntedHouse.Recognizer (Imperative, imperative, parser, fullParses)
import Control.Monad.Except (throwError)
import HauntedHouse.Build.GameState (buildGameState)
import qualified Data.Text
import HauntedHouse.Game.Narration (displaySceneM)
import HauntedHouse.Game.Engine (catchEngine)

data GameInput
    = MkGameInput Text
    | Verbose
    | Quit

start :: GameStateExceptT ()
start = buildGameState >> topLevel

topLevel :: GameStateExceptT ()
topLevel = get >>= _displayAction' >> inputAction

inputAction :: GameStateExceptT ()
inputAction = do
  input <- liftIO getInput
  case input of
    Nothing -> inputAction
    Just Verbose -> setVerbosityM >> topLevel
    Just Quit -> pass
    Just (MkGameInput input') -> do
      case runParser tokens input' of
          Left _ -> putStrLn "parse failed" >> displaySceneM True >> inputAction
          Right tokens' -> either throwError catchEngine (parseTokens tokens')
                            >> topLevel
  
getInput :: IO (Maybe GameInput)
getInput = do
  str <- runInputT defaultSettings go
  case str of
    ""     -> pure Nothing
    "quit" -> pure $ Just Quit
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