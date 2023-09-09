module HauntedHouse.Game.Engine.VerbPhraseThree where
import HauntedHouse.Recognizer.WordClasses (PrepPhrase, Verb)
import HauntedHouse.Game.Model.World (GameStateExceptT)
import HauntedHouse.Tokenizer (Lexeme(..))
import Control.Monad.Except (MonadError(..))
import HauntedHouse.Game.Engine.VerbPhraseThree.Look 

verbPhraseThree :: (Verb,PrepPhrase, PrepPhrase) -> GameStateExceptT ()
verbPhraseThree (LOOK,prep, prep') = print ("verbPhraseThree" :: Text) 
                                      >> doLookTwoPrepM (prep, prep')
verbPhraseThree _ = throwError "verbPhraseThree implementation incomplete"