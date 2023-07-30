module HauntedHouse.TopLevel  where 
import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Tokenizer (tokens, Lexeme, runParser)
import System.Console.Haskeline 
        (InputT, getInputLine, runInputT, defaultSettings)
import qualified Data.Char (toUpper)
import HauntedHouse.Recognizer (Imperative, imperative, parser, fullParses)
import HauntedHouse.Game.Location (getLocationId, getLocation)
import HauntedHouse.Game.Narration (displayScene)
import HauntedHouse.Game.Engine (engine)
import Control.Monad.Except (throwError)

data GameInput
    = MkGameInput Text
    | Quit

topLevel :: GameStateExceptT ()
topLevel = go
  where
    go :: GameStateExceptT ()
    go = getLocationId >>= 
          getLocation  >>=
          displayScene >> 
          go'

    go' :: GameStateExceptT () 
    go' = do 
      input <- liftIO getInput 
      case input of 
        Nothing -> go 
        Just Quit -> pass 
        Just (MkGameInput input') -> do
          case runParser tokens input' of
              Left _ -> putStrLn "parse failed" >> go
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
    _          -> Left ("Nonsense in" :: Text)
    where
      parsed = fst $ fullParses (parser imperative) toks

