module HauntedHouse.Game.Engine where

import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Engine.OnlyVerb ( evalOnlyVerb )

import HauntedHouse.Recognizer.WordClasses
        ( Imperative(..), VerbPhrase(..))

import Control.Monad.Except (MonadError(..))

import Prelude hiding (show)
import HauntedHouse.Clarifier (doReportM)

catchEngine :: Imperative -> GameStateExceptT ()
catchEngine parsed = do
  engine <- _evaluator' <$> get
  engine parsed `catchError` doReportM

primaryEvaluator :: Imperative -> GameStateExceptT ()
primaryEvaluator (ImperativeClause (OnlyVerb verb)) =   evalOnlyVerb verb
primaryEvaluator (ImperativeClause (VerbPhrase1 verb nounPhrase)) = do 
  evalVerbNounPhrase <- _evalVerbNounPhrase' <$> ask 
  evalVerbNounPhrase (verb,nounPhrase)
primaryEvaluator (ImperativeClause (VerbPhrase2 verb prepPhrase)) = do
  _evalVerbPrepPhrase <- _evalVerbPrepPhrase' <$> ask
  _evalVerbPrepPhrase (verb,prepPhrase)
primaryEvaluator _ = throwError "You need to be more clear"
{-
data Clarification = Clarification {
    _clarifyingLabel' :: Label Lexeme
  , _gidObjectPairs' :: NonEmpty (GID Object,Object)
}
-}
