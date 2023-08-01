module HauntedHouse.Game.Engine.VerbPhraseOneEvaluator where

import Control.Monad.Except (throwError)
import HauntedHouse.Recognizer (Verb, NounPhrase)
import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Tokenizer 
import HauntedHouse.Game.Engine.OnlyVerb.DoGo (doGo)

evalVerbPhrase1 :: (Verb, NounPhrase) -> GameStateExceptT ()
evalVerbPhrase1 (OPEN, _) = throwError "OPEN incomplete"
evalVerbPhrase1 (GO,mdir) = doGo mdir 
evalVerbPhrase1 (verb, _) = throwError (show verb <> " not evaluated")


