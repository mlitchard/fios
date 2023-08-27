module HauntedHouse.Game.Engine where

import HauntedHouse.Game.Model.World (GameStateExceptT, GameState (_engine'))
import HauntedHouse.Game.Engine.OnlyVerb ( evalOnlyVerb )
import HauntedHouse.Game.Engine.VerbPhraseOneEvaluator
    ( evalVerbPhrase1 )
import HauntedHouse.Recognizer.WordClasses
    ( Imperative(..), VerbPhrase(..) )
import HauntedHouse.Game.Engine.VerbPhraseTwo 
import Control.Monad.Except (MonadError(..))
import HauntedHouse.Clarifier (updateReport)

catchEngine :: Imperative -> GameStateExceptT ()
catchEngine parsed = do
  engine <- _engine' <$> get
  engine parsed `catchError` updateReport 

primaryEngine :: Imperative -> GameStateExceptT ()
primaryEngine (ImperativeClause (OnlyVerb verb)) = evalOnlyVerb verb
primaryEngine (ImperativeClause (VerbPhrase1 verb nounPhrase)) =
  evalVerbPhrase1 (verb,nounPhrase)
primaryEngine (ImperativeClause (VerbPhrase2 verb prepPhrase)) = 
  evalVerbPhrase2 (verb,prepPhrase)
primaryEngine (ClarifyingClause _) = throwError "You need to be more clear"