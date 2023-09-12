module HauntedHouse.Game.Engine.VerbPhraseSeven where
import HauntedHouse.Recognizer (PrepPhrase, NounPhrase, Verb)
import HauntedHouse.Game.Model.World (GameStateExceptT)
import HauntedHouse.Tokenizer (Lexeme(..))
import Control.Monad.Error.Class (MonadError(..))
import HauntedHouse.Game.Engine.VerbPhraseSeven.Open (doOpenNPPP)
import HauntedHouse.Game.Engine.VerbPhraseSeven.Close (doCloseNPPP)

verbPhraseSeven :: (Verb, NounPhrase, PrepPhrase) -> GameStateExceptT ()
verbPhraseSeven (OPEN, np, pp) = doOpenNPPP np pp
verbPhraseSeven (CLOSE, np, pp) = doCloseNPPP np pp
verbPhraseSeven _ = throwError "verbPhraseSeven incomplete" 
