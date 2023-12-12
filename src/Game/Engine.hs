{-# OPTIONS_GHC -Wno-type-defaults #-}
module Game.Engine where

import Game.Model.World
import Game.Engine.OnlyVerb ( evalOnlyVerb )
import Recognizer.WordClasses
        ( Imperative(..), VerbPhrase(..), AdjPhrase (..))
import Control.Monad.Except (MonadError(..))
import Prelude hiding (show)
import Clarifier (doReportM)
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
primaryEvaluator (ImperativeClause (VerbPhrase7 verb np pp)) = do
  eval <- _evalVerbPhraseSeven' <$> ask
  eval (verb,np,pp)
primaryEvaluator (ImperativeClause (VerbPhrase5 verb ap@(AdjNoun {}))) = do
  eval <- _evalVerbPhraseFive' <$> ask
  eval (verb, ap)

primaryEvaluator err = throwError (show err <> "You need to be more clear")

