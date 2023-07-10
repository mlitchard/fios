module HauntedHouse.Game.Engine where
import HauntedHouse.Game.GameState (GameStateExceptT)
import HauntedHouse.Game.Parser
import Control.Monad.Except (throwError)
import HauntedHouse.Game.Engine.VerbPhraseOneEvaluator

engine :: Imperative -> GameStateExceptT ()
engine (ImperativeClause (OnlyVerb verb)) = evalOnlyVerb verb
engine (ImperativeClause (VerbPhrase1 verb nounPhrase)) =
  evalVerbPhrase1 (verb,nounPhrase)
engine _ = throwError "verbPhrase2 not evaluated"