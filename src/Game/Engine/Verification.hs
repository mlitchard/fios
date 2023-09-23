module Game.Engine.Verification where
import Recognizer.WordClasses
    ( AdjPhrase(Adjective),
      Adjective,
      Imperative,
      NounPhrase(Noun, NounPhrase1, NounPhrase2),
      PrepPhrase(PrepPhrase1) )
import Game.Model.GID (GID)
import qualified Data.List.NonEmpty
import Game.Object (getObjectsFromLabelM, testPerceptability, capturePerceptiblesM)
import Game.World (makeErrorReport, findAnchoredTo)
import Game.Model.World
import Clarifier
    ( subObjectAgreement,
      checkProximity,
      findInDirectObject,
      findNoun, clarifyWhich)
import Game.Model.Mapping (Label(..), LabelToGIDListMapping (..))
import Control.Monad.Except (MonadError(..))
import qualified Data.Map.Strict
import Data.These (These(..))
import Tokenizer (Lexeme)


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

verifyAccessabilityNP :: NounPhrase -> GameStateExceptT (GID Object, Object)
verifyAccessabilityNP (NounPhrase1 _ (NounPhrase2 adj (Noun n))) =
  verifySimple (descriptiveLabel adj) (directObjectLabel n)
verifyAccessabilityNP (NounPhrase2 adj (Noun n)) =
  verifySimple (descriptiveLabel adj) (directObjectLabel n)
verifyAccessabilityNP _ = throwError "verifyAccessabilityAP unfinished"


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

type ResultAccessabilityNP =
  (Either (Label Object, NonEmpty GIDObjectPair) GIDObjectPair)

type ResultAccessabilityPP =
      GameStateExceptT (Either (GameStateExceptT ()) (GID Object, Object))

verifyAccessabilityAPNP :: AdjPhrase
                            -> NounPhrase
                            -> GameStateExceptT (GID Object, Object)
verifyAccessabilityAPNP (Adjective adj) (Noun noun) = do
  verifySimple (Label adj) (Label noun)
verifyAccessabilityAPNP _ _ = throwError "verifyAccessabilityAPNP unfinished"
-- (PrepPhrase1 _ (Noun noun))

verifyAccessabilityPP :: (Imperative -> GameStateExceptT ())
                          -> PrepPhrase
                          -> ResultAccessabilityPP
verifyAccessabilityPP clarifierM (PrepPhrase1 _ (Noun noun)) = do
  (LabelToGIDListMapping m) <- _objectLabelMap'
                                <$> (getLocationM =<< getLocationIdM)
  entities <- throwMaybeM nopeErr
              =<< capturePerceptiblesM
              =<< throwMaybeM nopeErr (Data.Map.Strict.lookup (Label noun) m)
  if Data.List.NonEmpty.length entities == 1
    then pure (Right (head entities))
    else do
      -- we don't know it's a container
     --  mapM_ (updateContainerDescriptionM prep) objects
       clarifyWhich' <- _clarifyWhich' <$> ask
       pure (Left (clarifyWhich' clarifierM (Label noun, entities)))
  where
  --  displayActionM = showPlayerActionM >> showEnvironmentM
    nopeErr = "You don't see a " <> toText noun <> " here."
verifyAccessabilityPP clarifierM (PrepPhrase1 _ (NounPhrase2 adj (Noun noun))) = do
  (LabelToGIDListMapping m) <- _objectLabelMap'
                                <$> (getLocationM =<< getLocationIdM)
  entities <- throwMaybeM nopeErr
              =<< capturePerceptiblesM
              =<< throwMaybeM nopeErr (Data.Map.Strict.lookup (Label noun) m)
  let mres :: Maybe (NonEmpty (GID Object, Object))
      mres = nonEmpty $ Data.List.NonEmpty.filter
              (\(_,entity) -> matchAdjective (Label adj) entity) entities
  res <- throwMaybeM "You don't see that here." mres
  clarifyWhich' <-  _clarifyWhich' <$> ask
  pure $ if length res > 1
    then Left (clarifyWhich' clarifierM (Label noun, res))
    else Right (head res)
  where
    nopeErr = "You don't see a " <> toText noun <> " here."
verifyAccessabilityPP _ _ = throwError "verifyAccessabilityPP: evaluate not completed"

matchAdjective :: Label Adjective -> Object -> Bool
matchAdjective label (Object{..}) = label `elem` _descriptives'

verifySensibilityNPPP :: (Imperative -> GameStateExceptT ())
                          -> NounPhrase
                          -> PrepPhrase
                          -> GameStateExceptT (GID Object, Object)
verifySensibilityNPPP clarifyingM np pp = do
  possibleDirectObjects <- getObjectsFromLabelM (directObjectLabel (findNoun np))
  anchoredEntities <- throwMaybeM (makeErrorReport np pp)
                      $ nonEmpty
                      $ filter (checkProximity pp)
                      $ mapMaybe findAnchoredTo -- FIXME - not all indirect objects anchored
                      $ toList possibleDirectObjects
  possibleObjects@(object:|_) <- getObjectsFromLabelM indirectObjectLabel

  when (length possibleObjects > 1) $ do
    clarifyWhich' <- _clarifyWhich' <$> ask
    clarifyWhich' clarifyingM (indirectObjectLabel, possibleObjects)
  -- toList anchoredEntities
  allMatches@(matched:|_) <- throwMaybeM (makeErrorReport np pp)
                  $ nonEmpty
                  $ mapMaybe (subObjectAgreement (fst object))
                  $ Data.List.NonEmpty.toList anchoredEntities
  when (length allMatches > 1) $ do
    let pairMatches = _anchoredObject' <$> allMatches
    clarifyWhich' <- _clarifyWhich' <$> ask
    clarifyWhich' clarifyingM (directObjectLabel (findNoun np), pairMatches)
  pure (_anchoredObject' matched)
  where
    indirectObjectLabel  = Label $ findInDirectObject pp
