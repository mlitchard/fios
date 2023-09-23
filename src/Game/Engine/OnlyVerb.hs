module Game.Engine.OnlyVerb where
import Recognizer (Verb)
import Game.Model.World (GameStateExceptT)
import Tokenizer (Lexeme(..))
import Game.Engine.OnlyVerb.DoLook (doLookM)
import Control.Monad.Except (throwError)

evalOnlyVerb :: Verb -> GameStateExceptT ()
evalOnlyVerb VERBOSE = pass
evalOnlyVerb LOOK = doLookM 
evalOnlyVerb verb = throwError (show verb <> "not evaluated yet")