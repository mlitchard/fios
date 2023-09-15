module HauntedHouse.Game.Engine.VerbPhraseOneEvaluator where

import Control.Monad.Except (throwError)
import HauntedHouse.Recognizer (Verb, NounPhrase)
import HauntedHouse.Game.Model.World (GameStateExceptT)
import HauntedHouse.Tokenizer
import HauntedHouse.Game.Engine.OnlyVerb.DoGo (doGo)
import HauntedHouse.Clarifier (findNoun)
import Data.Text (toLower)
import HauntedHouse.Game.Engine.VerbPhraseOneEvaluator.Get (doGet)

evalVerbNounPhrase :: (Verb, NounPhrase) -> GameStateExceptT ()
evalVerbNounPhrase (OPEN, _) = throwError "OPEN incomplete"
evalVerbNounPhrase (GO,mdir) = doGo mdir
evalVerbNounPhrase (GET,np)  = doGet np 
evalVerbNounPhrase (LOOK,np) = throwError (lookError np)
evalVerbNounPhrase (verb, _) = throwError (show verb <> " not evaluated")

lookError :: NounPhrase -> Text
lookError np =
  "This ain't Zork. Try looking at, or in " <> toLower (show noun) <> "."
  where
    noun = findNoun np