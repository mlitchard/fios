{-# OPTIONS_GHC -Wno-type-defaults #-}
module HauntedHouse.Game.Engine.VerbPhraseThree.Look where
import HauntedHouse.Recognizer.WordClasses (PrepPhrase(PrepPhrase1), NounPhrase (..), Preposition)
import HauntedHouse.Game.Model.World
import Control.Monad.Except (throwError)
import HauntedHouse.Clarifier (findNoun, findAnchoredTo, findInDirectObject, clarifyingLookObjectM, subObjectAgreement, checkProximity, clarifyingLookSubjectM)
import HauntedHouse.Game.Model.Mapping (Label(..))
import HauntedHouse.Game.Object (getObjectsFromLabelM, getObjectM)
import qualified Data.List.NonEmpty
import HauntedHouse.Game.Model.Display (updateDisplayActionM, showPlayerActionM, showEnvironmentM, maybeDescribeNexusM, updateContainerDescriptionM)
import HauntedHouse.Game.Model.GID
import HauntedHouse.Game.Engine.Utilities (prepositionFromPhrase)

doLookTwoPrepM :: (PrepPhrase, PrepPhrase) -> GameStateExceptT ()
doLookTwoPrepM (PrepPhrase1 prep np,pp) = do
  gsub@(gid,_) <- verifyExistenceNPPP prep np pp
  updateContainerDescriptionM prep gsub
  updatedSubject <- getObjectM gid
  maybeDescribeNexusM (_mNexus' updatedSubject)
  updateDisplayActionM (showPlayerActionM >> showEnvironmentM)
doLookTwoPrepM _ = throwError "doLookTwoPrep implementation unfinished"

-- assumes entity is container
-- throws error if it isn't
-- and then maintainer needs to sort the bad logic that got this far

-- (ExceptT Text (StateT GameState IO)) 

{-
data NounPhrase
  = NounPhrase1 Determiner NounPhrase
  | NounPhrase3 Number NounPhrase
  | Noun Noun
  deriving stock (Show, Eq, Ord)
-}
-- FIXME . change data Object to data Entity everywhere

verifyExistenceNPPP :: Preposition
                        -> NounPhrase
                        -> PrepPhrase
                        -> GameStateExceptT (GID Object, Object)
verifyExistenceNPPP whatPrep np pp = do
  possibleSubjects <- getObjectsFromLabelM subjectLabel
  anchoredEntities <- throwMaybeM (makeErrorReport np pp)
                      $ nonEmpty
                      $ filter (checkProximity pp)
                      $ mapMaybe findAnchoredTo
                      $ toList possibleSubjects
  possibleObjects@(object:|_) <- getObjectsFromLabelM objectLabel

  when (length possibleObjects > 1) $ do
    clarifyWhich <- _clarifyWhich' <$> ask
    clarifyWhich clarifyingLookObjectM (objectLabel, possibleObjects)
  -- toList anchoredEntities
  allMatches@(matched:|_) <- throwMaybeM (makeErrorReport np pp)
                  $ nonEmpty
                  $ mapMaybe (subObjectAgreement (fst object))
                  $ Data.List.NonEmpty.toList anchoredEntities
  when (length allMatches > 1) $ do
    let pairMatches = _anchoredObject' <$> allMatches
    clarifyWhich <- _clarifyWhich' <$> ask
    clarifyWhich (clarifyingLookSubjectM whatPrep) (subjectLabel, pairMatches)
  pure (_anchoredObject' matched)
  where
    subjectLabel = Label $ findNoun np
    objectLabel  = Label $ findInDirectObject pp

makeErrorReport :: NounPhrase -> PrepPhrase -> Text
makeErrorReport _np _pp = "You don't see that here"