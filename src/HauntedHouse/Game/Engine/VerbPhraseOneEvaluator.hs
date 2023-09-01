module HauntedHouse.Game.Engine.VerbPhraseOneEvaluator where

import Control.Monad.Except (throwError)
import HauntedHouse.Recognizer (Verb, NounPhrase)
import HauntedHouse.Game.Model.World (GameStateExceptT)
import HauntedHouse.Tokenizer 
import HauntedHouse.Game.Engine.OnlyVerb.DoGo (doGo)

evalVerbNounPhrase :: (Verb, NounPhrase) -> GameStateExceptT ()
evalVerbNounPhrase (OPEN, _) = throwError "OPEN incomplete"
evalVerbNounPhrase (GO,mdir) = doGo mdir
evalVerbNounPhrase (verb, _) = throwError (show verb <> " not evaluated")