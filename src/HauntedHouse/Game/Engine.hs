module HauntedHouse.Game.Engine where

import Control.Monad.Except (throwError)
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine)

import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Game.Engine.OnlyVerb
import HauntedHouse.Game.Engine.VerbPhraseOneEvaluator
import HauntedHouse.Game.Location (getLocation, getLocationId)
import HauntedHouse.Game.Narration (displayScene)
import HauntedHouse.Recognizer
import HauntedHouse.Recognizer.WordClasses
import HauntedHouse.Tokenizer (tokens, runParser, Lexeme(..))
import qualified Data.Char
import Control.Arrow (ArrowLoop(loop))

engine :: Imperative -> GameStateExceptT ()
engine (ImperativeClause (OnlyVerb verb)) = evalOnlyVerb verb
engine (ImperativeClause (VerbPhrase1 verb nounPhrase)) =
  evalVerbPhrase1 (verb,nounPhrase)
engine _ = throwError "verbPhrase2 not evaluated"
{-
wrapper :: GameStateExceptT ()
wrapper = go
  where
    go :: GameStateExceptT ()
    go = getLocationId >>= getLocation >>= displayScene >> go'

    go' :: GameStateExceptT () 
    go' = do 
      input <- liftIO getInput 
      case input of 
        Nothing -> go 
        Just Quit -> pass 
        Just (MkGameInput input') -> do
          case runParser tokens input' of
              Left _ -> putStrLn "parse failed" >> go
              Right tokens' -> either throwError engine (parseTokens tokens') >> wrapper


  --  go = liftIO (getLocationId >>= getLocation >>= displayScene) >> getInput

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


topLevel :: [Lexeme] -> GameStateExceptT ()
topLevel tokens' = do
    let (parsed:_) = fst $ fullParses (parser imperative) tokens'
    (maybeError, gState) <- liftIO $ (runStateT . runExceptT) (engine parsed) initGameState
    case maybeError of
        (Left err) -> liftIO $ putStrLn ("Error: " <> toString err)
        (Right _)  -> mapM_ (putStrLn . toString) $ _report gState

-- earlyParser :: [Lexeme] -> GameStateExceptT 
format :: [Text] -> String
format = concatMap (\str -> toString str <> "\n")

engine :: Imperative -> GameStateExceptT ()
engine (ImperativeClause vp@(VerbPhrase1 OPEN np)) = tryOpen np

-- 
whichFunctionNP :: VerbPhrase -> (NounPhrase -> GameStateExceptT ())
whichFunctionNP (OnlyVerb v) = case v of
                            OPEN -> tryOpen
                            _    -> pass
whichFunctionNP (VerbPhrase1 v np) = pass
whichFunctionNP (VerbPhrase2 v (Preposition AT)) = case v of
                                                    LOOK -> tryLookAt
                                                    _    -> pass
whichFunctionNP (VerbPhrase2 v pp) = pass


verbPhraseOne :: VerbPhrase -> GameStateExceptT ()
verbPhraseOne (VerbPhrase1 v np) = case v of
                                    OPEN -> do
                                             tryOpen np `catchError` (\x -> updateReport [x])

verbPhraseOne _ = pass

verbWithPrepPhrase :: (Lexeme, PrepPhrase) -> GameStateExceptT ()
verbWithPrepPhrase (verb,pp) = case verb of
                                LOOK -> trySpecificLook pp
                                _    -> pass
-}