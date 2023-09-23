module Game.Engine.VerbPhraseThree where
import Recognizer.WordClasses (PrepPhrase, Verb)
import Game.Model.World (GameStateExceptT)
import Tokenizer (Lexeme(..))
import Control.Monad.Except (MonadError(..))
import Game.Engine.VerbPhraseThree.Look 

verbPhraseThree :: (Verb,PrepPhrase, PrepPhrase) -> GameStateExceptT ()
verbPhraseThree (LOOK,whatP, whereP) = doLookTwoPrepM (whatP, whereP)

verbPhraseThree _ = throwError "verbPhraseThree implementation incomplete"