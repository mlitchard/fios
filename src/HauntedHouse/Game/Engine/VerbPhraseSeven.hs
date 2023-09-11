module HauntedHouse.Game.Engine.VerbPhraseSeven where
import HauntedHouse.Recognizer (PrepPhrase, NounPhrase, Verb)
import HauntedHouse.Game.Model.World (GameStateExceptT)
import HauntedHouse.Tokenizer (Lexeme(..))
import Control.Monad.Error.Class (MonadError(..))
import HauntedHouse.Game.Engine.VerbPhraseSeven.Open (doOpenNPPP)

verbPhraseSeven :: (Verb, NounPhrase, PrepPhrase) -> GameStateExceptT ()
verbPhraseSeven (OPEN, np, pp) = doOpenNPPP np pp
verbPhraseSeven _ = throwError "verbPhraseSeven incomplete" 
