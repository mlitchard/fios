module HauntedHouse.Game.Engine.OnlyVerb where
import HauntedHouse.Recognizer (Verb)
import HauntedHouse.Game.GameState (GameStateExceptT)
import HauntedHouse.Tokenizer (Lexeme(..))
import HauntedHouse.Game.Engine.OnlyVerb.DoLook (doLook)
import Control.Monad.Except (throwError)

evalOnlyVerb :: Verb -> GameStateExceptT ()
evalOnlyVerb LOOK = doLook 
evalOnlyVerb verb = throwError (show verb <> "not evaluated yet")