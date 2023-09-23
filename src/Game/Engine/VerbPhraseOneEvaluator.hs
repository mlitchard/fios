module Game.Engine.VerbPhraseOneEvaluator where

import Control.Monad.Except (throwError)
import Recognizer (Verb, NounPhrase)
import Game.Model.World (GameStateExceptT)
import Tokenizer
import Game.Engine.OnlyVerb.DoGo (doGo)
import Clarifier (findNoun)
import Data.Text (toLower)
import Game.Engine.VerbPhraseOneEvaluator.Get (doGet)

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