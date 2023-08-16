module HauntedHouse.Game.Engine.VerbPhraseTwo where 
import HauntedHouse.Recognizer (PrepPhrase)
import HauntedHouse.Recognizer.WordClasses (Verb)
import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Tokenizer (Lexeme(..))
import HauntedHouse.Game.Engine.VerbPhraseTwoEvaluator.Look (doLookObjectM)
import Control.Monad.Error.Class (throwError)

evalVerbPhrase2 :: (Verb, PrepPhrase) -> GameStateExceptT ()
evalVerbPhrase2 (LOOK, prepPhrase) = doLookObjectM prepPhrase
evalVerbPhrase2 (verb,_) = 
  throwError (show verb <> " not evaulated in evalVerbPhrase2")