{-# OPTIONS_GHC -Wno-type-defaults #-}
module HauntedHouse.Game.Engine.VerbPhraseThree.Look where
import HauntedHouse.Recognizer.WordClasses (PrepPhrase(PrepPhrase1), Noun, NounPhrase (..))
import HauntedHouse.Tokenizer (Lexeme(..))
import HauntedHouse.Game.Model.World
import Control.Monad.Except (MonadError(..))
import HauntedHouse.Clarifier (findNoun, findAnchoredTo, findInDirectObject, clarifyingLookObjectM, subObjectAgreement, checkProximity, clarifyingLookSubjectM)
import HauntedHouse.Game.Model.Mapping (Label(..))
import HauntedHouse.Game.Object (getObjectGIDPairM, getObjectsFromLabelM)
import qualified Data.List.NonEmpty
import HauntedHouse.Game.Model.Display (describeObjectM, updateDisplayActionM, showPlayerActionM, showEnvironmentM)

doLookTwoPrepM :: (PrepPhrase, PrepPhrase) -> GameStateExceptT ()
doLookTwoPrepM (PrepPhrase1 AT noun,prep) = doLookAtPrepM (noun, prep)
doLookTwoPrepM _ = throwError "doLookTwoPrep implementation unfinished"

-- matchesProximity
{-
data NounPhrase
  = NounPhrase1 Determiner NounPhrase
  | NounPhrase3 Number NounPhrase
  | Noun Noun
  deriving stock (Show, Eq, Ord)
-}
-- FIXME . change data Object to data Entity everywhere
doLookAtPrepM :: (NounPhrase,PrepPhrase) -> GameStateExceptT ()
doLookAtPrepM (np,pp) = do
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
    let pairMatches = (\a -> (_anchoredGID' a, _anchoredObject' a)) 
                    <$> allMatches
    clarifyWhich <- _clarifyWhich' <$> ask 
    clarifyWhich clarifyingLookSubjectM (subjectLabel, pairMatches)
  describeObjectM (_anchoredObject' matched) 
    >> updateDisplayActionM (showPlayerActionM >> showEnvironmentM)
  where 
    subjectLabel = Label $ findNoun np
    objectLabel  = Label $ findInDirectObject pp

makeErrorReport :: NounPhrase -> PrepPhrase -> Text
makeErrorReport _np _pp = "You don't see that here"