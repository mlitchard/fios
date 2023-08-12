module HauntedHouse.TopLevel (start) where

import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Tokenizer (tokens, Lexeme, runParser)
import System.Console.Haskeline
        (InputT, getInputLine, runInputT, defaultSettings)
import qualified Data.Char (toUpper)
import HauntedHouse.Recognizer (Imperative, imperative, parser, fullParses)
import HauntedHouse.Game.Location (getLocationIdM, getLocationM)
import HauntedHouse.Game.Narration (displaySceneM)
import HauntedHouse.Game.Engine (engine)
import Control.Monad.Except (throwError)
import HauntedHouse.Build.GameState (buildGameState)
import qualified Data.Text

data GameInput
    = MkGameInput Text
    | Quit

start :: GameStateExceptT ()
start = buildGameState >> topLevel

topLevel :: GameStateExceptT ()
topLevel = displayScene'
  where
    displayScene' :: GameStateExceptT ()
    displayScene' = getLocationIdM >>= getLocationM  >>= displaySceneM >> go'

    go' :: GameStateExceptT ()
    go' = do
      input <- liftIO getInput
      case input of
        Nothing -> displayScene'
        Just Quit -> pass
        Just (MkGameInput input') -> do
          liftIO $ print (("input debug go' " :: String) <> show input')
          case runParser tokens input' of
              Left _ -> putStrLn "parse failed" >> displayScene'
              Right tokens' -> either throwError engine (parseTokens tokens')
                                >> topLevel

getInput :: IO (Maybe GameInput)
getInput = do
    str <- runInputT defaultSettings go
    case str of
        ""     -> pure Nothing
        "quit" -> pure $ Just Quit
        input  -> pure $ Just (mkGameInput input)
    where
        go :: InputT IO String
        go = do
           -- displayScene
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
