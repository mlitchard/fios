module HauntedHouse.Game.Engine where

import HauntedHouse.Game.Model.World (GameStateExceptT)
import HauntedHouse.Game.Engine.OnlyVerb ( evalOnlyVerb )
import HauntedHouse.Game.Engine.VerbPhraseOneEvaluator
    ( evalVerbPhrase1 )
import HauntedHouse.Recognizer.WordClasses
    ( Imperative(..), VerbPhrase(..) )
import HauntedHouse.Game.Engine.VerbPhraseTwo 

engine :: Imperative -> GameStateExceptT ()
engine (ImperativeClause (OnlyVerb verb)) = evalOnlyVerb verb
engine (ImperativeClause (VerbPhrase1 verb nounPhrase)) =
  evalVerbPhrase1 (verb,nounPhrase)
engine (ImperativeClause (VerbPhrase2 verb prepPhrase)) = 
  evalVerbPhrase2 (verb,prepPhrase)