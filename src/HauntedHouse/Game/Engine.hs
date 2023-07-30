module HauntedHouse.Game.Engine where

import Control.Monad.Except (throwError)

import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Game.Engine.OnlyVerb ( evalOnlyVerb )
import HauntedHouse.Game.Engine.VerbPhraseOneEvaluator
    ( evalVerbPhrase1 )
import HauntedHouse.Recognizer.WordClasses
    ( Imperative(..), VerbPhrase(VerbPhrase1, OnlyVerb) )

engine :: Imperative -> GameStateExceptT ()
engine (ImperativeClause (OnlyVerb verb)) = evalOnlyVerb verb
engine (ImperativeClause (VerbPhrase1 verb nounPhrase)) =
  evalVerbPhrase1 (verb,nounPhrase)
engine _ = throwError "verbPhrase2 not evaluated"
