module HauntedHouse.Game.Engine.OnlyVerb where
import HauntedHouse.Recognizer (Verb)
import HauntedHouse.Game.Model.World (GameStateExceptT)
import HauntedHouse.Tokenizer (Lexeme(..))
import HauntedHouse.Game.Engine.OnlyVerb.DoLook (doLookM)
import Control.Monad.Except (throwError)

evalOnlyVerb :: Verb -> GameStateExceptT ()
evalOnlyVerb VERBOSE = pass
evalOnlyVerb LOOK = doLookM 
evalOnlyVerb verb = throwError (show verb <> "not evaluated yet")