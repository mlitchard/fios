module HauntedHouse.Game.Engine.Verification where 
import HauntedHouse.Recognizer 
import HauntedHouse.Game.Model.GID (GID)
import qualified Data.List.NonEmpty
import HauntedHouse.Game.Object (getObjectsFromLabelM)
import HauntedHouse.Game.World (makeErrorReport, findAnchoredTo)
import HauntedHouse.Game.Model.World
import HauntedHouse.Clarifier
    ( subObjectAgreement,
      checkProximity,
      findInDirectObject,
      findNoun)  
import HauntedHouse.Game.Model.Mapping (Label(..))
-- clarifyingLookSubjectM :: Preposition -> Imperative -> GameStateExceptT ()
verifyExistenceNPPP :: (Imperative -> GameStateExceptT ())
                        -> NounPhrase
                        -> PrepPhrase
                        -> GameStateExceptT (GID Object, Object)
verifyExistenceNPPP clarifyingM np pp = do
  possibleSubjects <- getObjectsFromLabelM subjectLabel
  anchoredEntities <- throwMaybeM (makeErrorReport np pp)
                      $ nonEmpty
                      $ filter (checkProximity pp)
                      $ mapMaybe findAnchoredTo
                      $ toList possibleSubjects
  possibleObjects@(object:|_) <- getObjectsFromLabelM objectLabel

  when (length possibleObjects > 1) $ do
    clarifyWhich <- _clarifyWhich' <$> ask
    clarifyWhich clarifyingM (objectLabel, possibleObjects)
  -- toList anchoredEntities
  allMatches@(matched:|_) <- throwMaybeM (makeErrorReport np pp)
                  $ nonEmpty
                  $ mapMaybe (subObjectAgreement (fst object))
                  $ Data.List.NonEmpty.toList anchoredEntities
  when (length allMatches > 1) $ do
    let pairMatches = _anchoredObject' <$> allMatches
    clarifyWhich <- _clarifyWhich' <$> ask
    clarifyWhich clarifyingM (subjectLabel, pairMatches)
  pure (_anchoredObject' matched)
  where
    subjectLabel = Label $ findNoun np
    objectLabel  = Label $ findInDirectObject pp




