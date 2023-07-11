module HauntedHouse.Game.Engine.VerbPhraseOneEvaluator where

import Control.Monad.Except (throwError)
import HauntedHouse.Recognizer (Verb, NounPhrase)
import HauntedHouse.Game.GameState (GameStateExceptT)
import HauntedHouse.Tokenizer 

evalVerbPhrase1 :: (Verb, NounPhrase) -> GameStateExceptT ()
evalVerbPhrase1 (OPEN, _) = throwError "OPEN incomplete"
evalVerbPhrase1 (verb, _) = throwError (show verb <> " not evaluated")


