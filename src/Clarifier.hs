module Clarifier where

import Control.Monad.Except ( MonadError(throwError) )
import Game.Model.World
import Game.Model.GID (GID)
import Game.Model.Mapping (Label (..))
import Tokenizer (Lexeme)
import Game.Model.Display
        (updateEnvironmentM, showEnvironmentM
        , showPlayerActionM, updateDisplayActionM)
import Data.Text (toLower)
import Recognizer.WordClasses
        (Imperative (..) , PrepPhrase (..), Noun, Preposition)
import qualified Data.List.NonEmpty
import Game.Model.Condition (Proximity (..))
import Tokenizer.Data (Lexeme(..))
import Prelude hiding (show)
import Recognizer (NounPhrase(..))
import Game.Object (getObjectsFromLabelM)

doReportM :: Text -> GameStateExceptT ()
doReportM report' = do
  setReportM report'
  modify' (\gs -> gs{_displayAction' = displayReport})

setReportM :: Text -> GameStateExceptT ()
setReportM report' = do
  currentReport <- _report' <$> get
  modify' (\gs -> gs{_report' = currentReport <> [report']})

clearReportM :: GameStateExceptT () 
clearReportM = do 
  modify' (\gs -> gs{_report' = mempty})

samePageCheckM :: Label Object -> Label Object -> GameStateExceptT ()
samePageCheckM label label'
  | label == label' = pass
  | otherwise       = throwError errMsg
  where
    errMsg = "What were we talking about again?"

displayReport :: GameStateExceptT ()
displayReport = do
  mapM_ print . _report' =<< get

clarifyWhich :: ClarifyWhich
clarifyWhich f labelObjectPair@(Label label', objects) = do
  updateEnvironmentM preamble
    >> mapM_ objectOrientation  objects
    >> modify' (\gs -> gs {_clarification' = Just clarification})
    >> setEvaluatorM f -- clarifyingLookSubjectM
    >> updateDisplayActionM (showPlayerActionM >> showEnvironmentM)
  where
    clarification = Clarification {
        _clarifyingLabel' = fst labelObjectPair
      , _gidObjectPairs' = objects
    }
    preamble = "which " <> (toLower . toText) label' <> " do you mean?"

setEvaluatorM :: (Imperative -> GameStateExceptT ()) -> GameStateExceptT ()
setEvaluatorM engine = modify' (\gs -> gs{_evaluator' = engine})
{-
objectOrientation :: Object -> GameStateExceptT ()
objectOrientation (Object {..}) =
  describeOrientationM ("The " <> _shortName') _orientation'
-}
checkProximity :: PrepPhrase -> FoundAnchoredTo -> Bool
checkProximity prep (FoundAnchoredTo _ (_,prox)) =
  matchesProximity (prox,prep)

clarifyNotThere :: GameStateExceptT ()
clarifyNotThere = throwError "You don't see that here"

clarifyingM :: Imperative -> GameStateExceptT () 
clarifyingM (ClarifyingClause1 (NounPhrase1 _ (Noun sub)) prep) = pass {- do
  clarified <- throwMaybeM noClarityMSG . _clarification' =<< get
  samePageCheckM (Label sub) (_clarifyingLabel' clarified)
  objectListIobj <- getObjectsFromLabelM (Label (findInDirectObject prep))
  subjects <- throwMaybeM tryAgain
        $ nonEmpty $ filter (checkProximity prep) $ catMaybes
        $ Data.List.NonEmpty.toList
        $ Data.List.NonEmpty.map findAnchoredTo (_gidObjectPairs' clarified)
  
  obj <- if length objectListIobj == 1
          then pure $ head objectListIobj
          else throwError "error with length objectListIobj"
  matchedSubjects <- throwMaybeM "nonsense"
                      $ nonEmpty
                      $ mapMaybe (subObjectAgreement (fst obj))
                      $ Data.List.NonEmpty.toList subjects
  unless (length matchedSubjects == 1) $ throwError "several matched subjects"
  let gsub = (_anchoredObject' . head) matchedSubjects
  modify'(\gs -> gs{_clarifiedDirectObject' = Just gsub})
  where
    tryAgain = "Try that again"
    noClarityMSG = "Programmer Error: No clarifying object list"
-}
clarifyingM _ = throwError "clarifyingM implimentation not completed"

clarifyingLookDirectObjectM :: Preposition -> Imperative -> GameStateExceptT ()
clarifyingLookDirectObjectM whatPrep imperative = pass {- do
  clarifyingM imperative
  gsub@(gid,_) <- throwMaybeM errMSG . _clarifiedDirectObject' =<< get
  -- updatedDirectObject <- getObjectM gid
  maybeDescribeNexusM (_mNexus' updatedDirectObject)
  updateDisplayActionM 
    (showPlayerActionM >> showEnvironmentM >> describeObjectM gid)
  
  primaryEvaluator <- _primaryEvaluator' <$> ask
  setEvaluatorM primaryEvaluator
  where 
    errMSG = "clarifyingLookDirectObjectM error: missing clarified direct object"

-}
clarifyingLookIndirectObjectM :: Imperative -> GameStateExceptT () 
clarifyingLookIndirectObjectM _ = throwError "clarifyingLookObjectM not implemented"

clarifyingOpenDirectObjectM :: Imperative -> GameStateExceptT () 
clarifyingOpenDirectObjectM imperative = pass {- do
  clarifyingM imperative
  gsub@(gid,directObject) <- throwMaybeM errMSG . _clarifiedDirectObject' 
                              =<< get
  interface <- getContainerInterfaceM directObject
  _openAction' interface
  pass
  where 
    errMSG = "clarifyingOpenDirectObjectM error: missing clarified direct object"
-}
subObjectAgreement :: GID Object -> FoundAnchoredTo -> Maybe FoundAnchoredTo
subObjectAgreement gid' fat@(FoundAnchoredTo _ (gid,_))
  | gid == gid' = Just fat
  | otherwise   = Nothing

findInDirectObject :: PrepPhrase -> Noun
findInDirectObject (PrepPhrase1 _ np) = findNoun np
findInDirectObject (PrepPhrase2 _ _ _ np) = findNoun np

findNoun :: NounPhrase -> Noun
findNoun (Noun n) = n
findNoun (NounPhrase1 _ np) = findNoun np
findNoun (NounPhrase3 _ np) = findNoun np

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