module HauntedHouse.Clarifier where

import Control.Monad.Except ( MonadError(throwError) )
import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Mapping (Label (..))
import HauntedHouse.Tokenizer (Lexeme)
import HauntedHouse.Game.Model.Display
        (describeOrientationM, updateEnvironmentM, showEnvironmentM
        , showPlayerActionM, updateDisplayActionM, describeObjectM)
import Data.Text (toLower)
import HauntedHouse.Recognizer.WordClasses
        (Imperative (..) , PrepPhrase (..), Noun)
import qualified Data.List.NonEmpty
import HauntedHouse.Game.Model.Condition (Proximity (..))
import HauntedHouse.Tokenizer.Data (Lexeme(..))
import Prelude hiding (show)
import Text.Show (Show(..))
import HauntedHouse.Recognizer (NounPhrase(..))
import HauntedHouse.Game.Object (getObjectsFromLabelM)
-- import HauntedHouse.Game.Engine (primaryEngine)

doReportM :: Text -> GameStateExceptT ()
doReportM report' = do
  setReportM report'
  modify' (\gs -> gs{_displayAction' = displayReport})

setReportM :: Text -> GameStateExceptT ()
setReportM report' = do
  currentReport <- _report' <$> get
  modify' (\gs -> gs{_report' = currentReport <> [report']})

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
clarifyWhich f labelObjectPair@(Label label', objects) =
  updateEnvironmentM preamble
    >> mapM_ (\(_,object) -> objectOrientation object) objects
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
setEvaluatorM engine = do
  modify' (\gs -> gs{_evaluator' = engine})

objectOrientation :: Object -> GameStateExceptT ()
objectOrientation (Object shortName _ _ _ _ orientation _) =
  describeOrientationM ("The " <> shortName) orientation

clarifyNotThere :: GameStateExceptT ()
clarifyNotThere = throwError "You don't see that here"

clarifyingLookSubjectM :: Imperative -> GameStateExceptT ()
clarifyingLookSubjectM (ClarifyingClause1
                        (NounPhrase1 _ (Noun sub))
                        prep) = do
  clarified <- throwMaybeM noClarityMSG . _clarification' =<< get
  samePageCheckM (Label sub) (_clarifyingLabel' clarified)
  objectListIobj <- getObjectsFromLabelM (Label (findInDirectObject prep))
  mapM_ (\(gid,obj) -> print (_shortName' obj) >> print gid) objectListIobj -- Remove DEBUG
  subjects <- throwMaybeM tryAgain
        $ nonEmpty $ filter (checkProximity prep) $ catMaybes
        $ Data.List.NonEmpty.toList
        $ Data.List.NonEmpty.map findAnchoredTo (_gidObjectPairs' clarified)

  -- mapM (checkProximityM prep) x 
  obj <- if length objectListIobj == 1
          then pure $ head objectListIobj
          else throwError "error with length objectListIobj"
  matchedSubjects <- throwMaybeM "nonsense"
                      $ nonEmpty
                      $ mapMaybe (subObjectAgreement (fst obj))
                      $ Data.List.NonEmpty.toList subjects
  unless (length matchedSubjects == 1) $ throwError "several matched subjects"
  describeObjectM $ (_anchoredObject' . head) matchedSubjects
  primaryEvaluator <- _primaryEvaluator' <$> ask
  setEvaluatorM primaryEvaluator
  where
    tryAgain = "Try that again"
    noClarityMSG = "Programmer Error: No clarifying object list"
clarifyingLookSubjectM imp@(ImperativeClause _) = do
                                                    print ("primary engine to evaluate" :: Text)
                                                    primaryEngine <- _primaryEvaluator' <$> ask
                                                    primaryEngine imp
clarifyingLookSubjectM _ = throwError "clarifyingLook implementation unfinished"

clarifyingLookObjectM :: Imperative -> GameStateExceptT () 
clarifyingLookObjectM _ = throwError "clarifyingLookObjectM not implemented"

subObjectAgreement :: GID Object -> FoundAnchoredTo -> Maybe FoundAnchoredTo
subObjectAgreement gid' fat@(FoundAnchoredTo _ _ (gid,_))
  | gid == gid' = Just fat
  | otherwise   = Nothing

checkProximity :: PrepPhrase -> FoundAnchoredTo -> Bool
checkProximity prep (FoundAnchoredTo _ _ (_,prox)) =
  matchesProximity (prox,prep)

findInDirectObject :: PrepPhrase -> Noun
findInDirectObject (PrepPhrase1 _ np) = findNoun np
findInDirectObject (PrepPhrase2 _ _ _ np) = findNoun np

findNoun :: NounPhrase -> Noun
findNoun (Noun n) = n
findNoun (NounPhrase1 _ np) = findNoun np
findNoun (NounPhrase3 _ np) = findNoun np

  -- | AnchoredTo' (GID Object, Proximity) 
findAnchoredTo :: (GID Object, Object) -> Maybe FoundAnchoredTo
findAnchoredTo object = case object of
  (gid,obj@(Object _ _ _ _ _  (AnchoredTo' gp) _ )) -> Just $ FoundAnchoredTo
                                                                gid obj gp
  _ -> Nothing
{-
findAnchoredTo object@(_,Object _ _ _ _ _  (AnchoredTo' _) _) = Just object
findAnchoredTo _                                              = Nothing
-}
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