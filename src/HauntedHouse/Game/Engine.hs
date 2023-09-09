{-# OPTIONS_GHC -Wno-type-defaults #-}
module HauntedHouse.Game.Engine where

import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Engine.OnlyVerb ( evalOnlyVerb )

import HauntedHouse.Recognizer.WordClasses
        ( Imperative(..), VerbPhrase(..))

import Control.Monad.Except (MonadError(..))

import Prelude hiding (show)
import HauntedHouse.Clarifier (doReportM)
import Relude

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
  eval <- _evalVerbPrepPhrase' <$> ask
  eval (verb,prepPhrase)
primaryEvaluator (ImperativeClause (VerbPhrase3 verb prepOne prepTwo)) = do 
  eval <- _evalVerbTwoPrepPhrases' <$> ask
  eval (verb,prepOne,prepTwo) 
primaryEvaluator err = do 
                          print (show err)
                          throwError "You need to be more clear"
{-
data Clarification = Clarification {
    _clarifyingLabel' :: Label Lexeme
  , _gidObjectPairs' :: NonEmpty (GID Object,Object)
}
-}
