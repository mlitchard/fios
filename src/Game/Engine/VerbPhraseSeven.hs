module Game.Engine.VerbPhraseSeven where
import Recognizer (PrepPhrase, NounPhrase, Verb)
import Game.Model.World (GameStateExceptT)
import Tokenizer (Lexeme(..))
import Control.Monad.Error.Class (MonadError(..))
import Game.Engine.VerbPhraseSeven.Open (doOpenNPPP)
import Game.Engine.VerbPhraseSeven.Close (doCloseNPPP)

verbPhraseSeven :: (Verb, NounPhrase, PrepPhrase) -> GameStateExceptT ()
verbPhraseSeven (OPEN, np, pp) = doOpenNPPP np pp
verbPhraseSeven (CLOSE, np, pp) = doCloseNPPP np pp
verbPhraseSeven _ = throwError "verbPhraseSeven incomplete" 
