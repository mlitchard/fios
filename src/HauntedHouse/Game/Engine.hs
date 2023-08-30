module HauntedHouse.Game.Engine where

import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Engine.OnlyVerb ( evalOnlyVerb )
import HauntedHouse.Game.Engine.VerbPhraseOneEvaluator
    ( evalVerbPhrase1 )
import HauntedHouse.Recognizer.WordClasses
        ( Imperative(..), VerbPhrase(..), NounPhrase (..), PrepPhrase (..), Noun)
import HauntedHouse.Game.Engine.VerbPhraseTwo
import Control.Monad.Except (MonadError(..))
import HauntedHouse.Clarifier (updateReport)
import HauntedHouse.Game.Engine.Utilities (prepToProximity)
import HauntedHouse.Game.Object (getObjectsFromLabel)
import HauntedHouse.Game.Model.Mapping (Label(..))
import HauntedHouse.Game.Model.Condition (Proximity (..))
import HauntedHouse.Tokenizer.Data (Lexeme(..))
import Data.Profunctor.Rep (prepAdj)
import qualified Data.List.NonEmpty
import HauntedHouse.Game.Model.GID (GID)

catchEngine :: Imperative -> GameStateExceptT ()
catchEngine parsed = do
  engine <- _engine' <$> get
  engine parsed `catchError` updateReport

primaryEngine :: Imperative -> GameStateExceptT ()
primaryEngine (ImperativeClause (OnlyVerb verb)) =   evalOnlyVerb verb
primaryEngine (ImperativeClause (VerbPhrase1 verb nounPhrase)) =
  evalVerbPhrase1 (verb,nounPhrase)
primaryEngine (ImperativeClause (VerbPhrase2 verb prepPhrase)) =
  evalVerbPhrase2 (verb,prepPhrase)
primaryEngine _ = throwError "You need to be more clear"


{-
data Imperative
  = ImperativeClause VerbPhrase
  | ClarifyingClause1 NounPhrase PrepPhrase 
  | ClarifyingClause2 NounPhrase PrepPhrase PrepPhrase
  | ClarifyingClause3 NounPhrase PrepPhrase PrepPhrase PrepPhrase
  | ClarifyingClause4 AdjPhrase
  | ClarifyingClause5 AdjPhrase PrepPhrase
  | ClarifyingClause6 AdjPhrase PrepPhrase  PrepPhrase 
-}
clarifyingLookSubject :: Imperative -> GameStateExceptT ()
clarifyingLookSubject (ClarifyingClause1
                        (NounPhrase1 _ (Noun sub))
                        (PrepPhrase1 prep (NounPhrase1 _ (Noun iobj)))) = do
  -- proximity <- throwMaybeM "not a prep" $ prepToProximity prep
  objectListSub <- getObjectsFromLabel (Label sub)
  objectListIobj <- getObjectsFromLabel (Label iobj)
  let x = catMaybes
            $ Data.List.NonEmpty.toList
            $ Data.List.NonEmpty.map findAnchoredTo objectListSub

  obj <- if length objectListIobj == 1
          then pure $ head objectListIobj
          else throwError "You'll have to be more specific"
  pass
clarifyingLookSubject _ = throwError "clarifyingLook implementation unfinished"

findAnchoredTo :: (GID Object, Object) -> Maybe (GID Object,Object)
findAnchoredTo object@(_,Object _ _ _ _ _  (AnchoredTo' _) _) = Just object
findAnchoredTo _                                              = Nothing

isAnchoredTo :: Orientation -> Bool
isAnchoredTo (AnchoredTo' _) = True
isAnchoredTo _               = False

matchesProximity :: (Proximity,PrepPhrase) -> Bool
matchesProximity (PlacedOn, pp) =
  case pp of
    (PrepPhrase1 ON _)      -> True
    (PrepPhrase2 ON _ _ _)  -> True
    _                       -> False
matchesProximity (PlacedUnder, pp) =
  case pp of
    (PrepPhrase1 UNDER _)     -> True
    (PrepPhrase2 UNDER _ _ _) -> True
    _                         -> False
matchesProximity (PlacedAbove, pp) =
  case pp of
    (PrepPhrase1 ABOVE _)     -> True
    (PrepPhrase2 ABOVE _ _ _) -> True
    _                         -> False
matchesProximity (PlacedLeft, pp) =
  case pp of
    (PrepPhrase1 prep (NounPhrase1 _ (Noun LEFT))) -> cPrepOnTo prep
    _                                              -> False
matchesProximity (PlacedRight, pp) =
  case pp of
    (PrepPhrase1 prep (NounPhrase1 _ (Noun RIGHT))) -> cPrepOnTo prep
    _                                               -> False
matchesProximity (PlacedFront, pp) =
  case pp of
    (PrepPhrase1 prep (NounPhrase1 _ (Noun FRONT))) -> cPrepInTo prep
    _                                               -> False
matchesProximity (PlacedBehind, PrepPhrase1 BEHIND (NounPhrase1 _ _))  = True
matchesProximity _ = False

{-
  | PlacedLeft
  | PlacedRight
  | PlacedFront 
  | PlacedBack
-}

cPrepOnTo :: Lexeme -> Bool
cPrepOnTo prep
      | prep == ON = True
      | prep == TO = True
      | otherwise  = False

cPrepInTo :: Lexeme -> Bool
cPrepInTo prep
  | prep == IN = True
  | prep == TO = True
  | otherwise  = False