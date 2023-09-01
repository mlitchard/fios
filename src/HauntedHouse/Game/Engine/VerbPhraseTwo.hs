module HauntedHouse.Game.Engine.VerbPhraseTwo where 
import HauntedHouse.Recognizer (PrepPhrase)
import HauntedHouse.Recognizer.WordClasses (Verb)
import HauntedHouse.Game.Model.World (GameStateExceptT)
import HauntedHouse.Tokenizer (Lexeme(..))
import HauntedHouse.Game.Engine.VerbPhraseTwoEvaluator.Look (doLookObjectM)
import Control.Monad.Error.Class (throwError)

evalVerbPrepPhrase :: (Verb, PrepPhrase) -> GameStateExceptT ()
evalVerbPrepPhrase (LOOK, prepPhrase) = doLookObjectM prepPhrase
evalVerbPrepPhrase (verb,_) = 
  throwError (show verb <> " not evaulated in evalVerbPhrase2")