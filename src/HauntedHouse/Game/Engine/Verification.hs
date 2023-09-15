module HauntedHouse.Game.Engine.Verification where
import HauntedHouse.Recognizer
import HauntedHouse.Game.Model.GID (GID)
import qualified Data.List.NonEmpty
import HauntedHouse.Game.Object (getObjectsFromLabelM, testPerceptability)
import HauntedHouse.Game.World (makeErrorReport, findAnchoredTo)
import HauntedHouse.Game.Model.World
import HauntedHouse.Clarifier
    ( subObjectAgreement,
      checkProximity,
      findInDirectObject,
      findNoun)
import HauntedHouse.Game.Model.Mapping (Label(..))
import Control.Monad.Except (MonadError(..))
import HauntedHouse.Tokenizer.Data (Lexeme)

verifySimple :: Label Adjective 
                              -> Label Object 
                              -> GameStateExceptT (GID Object, Object)
verifySimple descriptiveLabel' directObjectLabel' = do
  possibleDirectObjects <- getObjectsFromLabelM directObjectLabel'
  testableEntity <- if length possibleDirectObjects > 1
    then throwError "verifyExistenceAP can't differentiate yet"
    else pure (head possibleDirectObjects)
  let descriptives = _descriptives' (snd testableEntity)
  -- does it match description?
  unless (matchDescriptive descriptiveLabel' descriptives)
    $ throwError noSeeMSG
  -- can player see it?
  unless (testPerceptability testableEntity)
    $ throwError noSeeMSG
  -- tests pass
  pure testableEntity
  where
    noSeeMSG = "You don't see that here."
-- (AdjNoun _ (Adjective adj) (Noun noun))

verifyAccessability :: NounPhrase -> GameStateExceptT (GID Object, Object)
verifyAccessability (NounPhrase1 _ (NounPhrase2 (Adjective adj) (Noun n))) = 
  verifySimple (descriptiveLabel adj) (directObjectLabel n)
verifyAccessability (NounPhrase2 (Adjective adj) (Noun n)) = 
  verifySimple (descriptiveLabel adj) (directObjectLabel n)
verifyAccessability _ = throwError "verifyAccessabilityAP unfinished"
  {-
  possibleDirectObjects <- getObjectsFromLabelM directObjectLabel
  testableEntity <- if length possibleDirectObjects > 1
    then throwError "verifyExistenceAP can't differentiate yet"
    else pure (head possibleDirectObjects)
  let descriptives = _descriptives' (snd testableEntity)
  -- does it match description?
  unless (matchDescriptive descriptiveLabel descriptives)
    $ throwError noSeeMSG
  -- can player see it?
  unless (testPerceptability testableEntity)
    $ throwError noSeeMSG
  -- tests pass
  pure testableEntity
  -}

descriptiveLabel :: Lexeme -> Label Adjective
descriptiveLabel = Label

directObjectLabel :: Lexeme -> Label Object
directObjectLabel = Label
-- verifyAccessabilityAP _ = throwError "verifyExistenceAP unfinished"

matchDescriptive :: Label Adjective -> [Label Adjective] -> Bool
matchDescriptive  testDescriptive descriptives =
  testDescriptive `elem` descriptives

{-
data Orientation 
  = ContainedBy' ContainedBy 
  | Inventory 
  | Floor (GID Object)
  | AnchoredTo' (GID Object, Proximity) 
  | Anchoring RoomAnchor
    deriving stock Show 
-}

{-
tryGetFromGIDM (ContainedBy' (ContainedBy {..})) = pass 
tryGetFromGIDM Inventory = throwError "You already have that."
tryGetFromGIDM
-} 
--- FIXME perceptibility
verifyExistenceNPPP :: (Imperative -> GameStateExceptT ())
                        -> NounPhrase
                        -> PrepPhrase
                        -> GameStateExceptT (GID Object, Object)
verifyExistenceNPPP clarifyingM np pp = do
  possibleDirectObjects <- getObjectsFromLabelM directObjectLabel
  anchoredEntities <- throwMaybeM (makeErrorReport np pp)
                      $ nonEmpty
                      $ filter (checkProximity pp)
                      $ mapMaybe findAnchoredTo
                      $ toList possibleDirectObjects
  possibleObjects@(object:|_) <- getObjectsFromLabelM indirectObjectLabel

  when (length possibleObjects > 1) $ do
    clarifyWhich <- _clarifyWhich' <$> ask
    clarifyWhich clarifyingM (indirectObjectLabel, possibleObjects)
  -- toList anchoredEntities
  allMatches@(matched:|_) <- throwMaybeM (makeErrorReport np pp)
                  $ nonEmpty
                  $ mapMaybe (subObjectAgreement (fst object))
                  $ Data.List.NonEmpty.toList anchoredEntities
  when (length allMatches > 1) $ do
    let pairMatches = _anchoredObject' <$> allMatches
    clarifyWhich <- _clarifyWhich' <$> ask
    clarifyWhich clarifyingM (directObjectLabel, pairMatches)
  pure (_anchoredObject' matched)
  where
    directObjectLabel = Label $ findNoun np
    indirectObjectLabel  = Label $ findInDirectObject pp




