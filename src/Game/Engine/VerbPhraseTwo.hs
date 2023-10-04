module Game.Engine.VerbPhraseTwo where 
import Recognizer (PrepPhrase)
import Recognizer.WordClasses (Verb)
import Game.Model.World (GameStateExceptT)
import Tokenizer (Lexeme(..))
import Game.Engine.VerbPhraseTwoEvaluator.Look (evalLookObjectM)
import Control.Monad.Error.Class (throwError)

evalVerbPrepPhrase :: (Verb, PrepPhrase) -> GameStateExceptT ()
evalVerbPrepPhrase (LOOK, prepPhrase) = evalLookObjectM prepPhrase
evalVerbPrepPhrase (verb,_) = 
  throwError (show verb <> " not evaulated in evalVerbPhrase2")