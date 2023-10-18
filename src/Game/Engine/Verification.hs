module Game.Engine.Verification where
import Recognizer.WordClasses
    ( AdjPhrase(Adjective),
      Adjective,
      Imperative,
      NounPhrase(Noun, NounPhrase1, NounPhrase2),
      PrepPhrase(..), Preposition )
import Game.Model.GID (GID)
import qualified Data.List.NonEmpty
import Game.Object (getObjectsFromLabelM, getObjectM, getObjectGIDPairM)
import Game.World (makeErrorReport)
import Game.Model.World as FoundObject (FoundObject (..))
import Game.Model.World
import Clarifier
    ( subObjectAgreement,
      checkProximity,
      findInDirectObject,
      findNoun, matchesProximity)
import Game.Model.Mapping (Label(..), LabelToGIDListMapping (..), GIDList)
import Control.Monad.Except (MonadError(..))
import Game.Engine.Utilities (descriptiveLabel, directObjectLabel)
import Data.Map.Strict (lookup)
import Tokenizer (Lexeme (..))
import Game.Scene (tryDisplayF)
import Data.Time.Calendar.OrdinalDate (fromSundayStartWeek)
import Game.Model.Condition (Proximity (..))

matchObjects :: FoundObject
                  -> FoundObject
                  -> GameStateExceptT (Maybe Proximity)
matchObjects advObject adjObject = do
  res <- tryAnchoredTo advOrientation
  pure $ case res of
    Nothing -> Nothing
    Just (AnchoredTo anchoredGid proximity)
      | anchoredGid == adjGid -> Just proximity
      | otherwise -> Nothing
  where
    advOrientation = _orientation' (FoundObject._entity' advObject)
    adjGid =  FoundObject._gid' adjObject

tryAnchoredTo :: Orientation -> GameStateExceptT (Maybe AnchoredTo)
tryAnchoredTo (AnchoredTo' f) = Just <$> f
tryAnchoredTo _ = pure Nothing

evaluatePossibleObjects :: NonEmpty FoundObject 
                            -> Maybe (NonEmpty FoundObject)
evaluatePossibleObjects possibles =
  let entities = evaluatePossibleObject <$> possibles
  in nonEmpty $ catMaybes $ toList entities

evaluatePossibleObject :: FoundObject -> Maybe FoundObject
evaluatePossibleObject (FoundObject gid entity) =
      let res = tryDisplayF entity
      in FoundObject gid <$> res

identifyPossiblelObjects :: NounPhrase
                                  -> GameStateExceptT (Label Object, Maybe PossibleObjects)

identifyPossiblelObjects (NounPhrase1 _ (Noun noun)) = do
  (LabelToGIDListMapping m) <- _objectLabelMap'
                                <$> (getLocationM =<< getLocationIdM)
  case Data.Map.Strict.lookup (Label noun) m of
    Nothing -> pure (Label noun,Nothing)
    Just (gid:|[]) -> do
                      objPair <- getObjectGIDPairM gid
                      pure (Label noun, Just (Found (makeFoundObject objPair)))
    Just gids -> do
                  objPairs <- mapM getObjectGIDPairM gids
                  let foundObjects = fmap makeFoundObject objPairs
                  pure (Label noun,Just (Possibles foundObjects))

identifyPossiblelObjects (NounPhrase2 adj (Noun noun)) = do
  (LabelToGIDListMapping m) <- _objectLabelMap'
                                <$> (getLocationM =<< getLocationIdM)
  case Data.Map.Strict.lookup (Label noun) m of
        Nothing -> pure (Label noun,Nothing)
        Just (gid:|[]) -> do
                            objPair <- getObjectGIDPairM gid
                            let found = makeFoundObject objPair
                            pure (Label noun,Just (Found found))
        Just gids -> do
                    res <- matchDescriptor (Label adj) gids
                    pure (Label noun, res)

identifyPossiblelObjects (Noun noun) = do
    (LabelToGIDListMapping m) <- _objectLabelMap'
                                  <$> (getLocationM =<< getLocationIdM)
    case Data.Map.Strict.lookup (Label noun) m of
      Nothing -> pure (Label noun, Nothing)
      Just (gid:|[]) -> do
                          objPair <- getObjectGIDPairM gid
                          let found = makeFoundObject objPair
                          pure (Label noun, Just (Found found))
      Just _ -> throwError "identifyPossiblelObjects incomplete" -- pure (Label noun, Just (Possibles xs))

identifyPossiblelObjects _ = throwError "identifyPossiblelObjects unfinished"

matchDescriptor :: Label Adjective
                    -> GIDList Object
                    -> GameStateExceptT (Maybe PossibleObjects)
matchDescriptor adj entityGids = do
  entities <- mapM getObjectGIDPairM entityGids
  let matched = nonEmpty $ Data.List.NonEmpty.filter
                (\(_,Object {..}) -> adj `elem` _descriptives') entities
  pure $ case matched of
                Nothing        -> Nothing
                Just (x :| []) -> Just (Found $ makeFoundObject x)
                Just xs        -> Just (Possibles $ makeFoundObject <$> xs)

makeFoundObject :: (GID Object, Object) -> FoundObject
makeFoundObject (gid, entity) = FoundObject {
    _gid' = gid
  , _entity' = entity
}

matchesProximity :: Proximity -> Lexeme -> Bool
matchesProximity PlacedOn ON = True
matchesProximity PlacedUnder UNDER = True
matchesProximity PlacedAbove ABOVE = True
matchesProximity PlacedLeft LEFT = True
matchesProximity PlacedRight RIGHT = True
matchesProximity PlacedFront FRONT = True
matchesProximity PlacedBehind BEHIND = True
matchesProximity _ _ = False

{-
verifySimple :: Label Adjective
                              -> Label Object
                              -> GameStateExceptT (GID Object, Object)
verifySimple descriptiveLabel' directObjectLabel' = do
  possibleDirectObjects <- getObjectsFromLabelM directObjectLabel'
  testableEntity <- if length possibleDirectObjects > 1
    then throwError "verifySimple can't differentiate yet"
    else pure (head possibleDirectObjects)
  let descriptives = Game.Model.World._descriptives' (snd testableEntity)
  -- does it match description?
  unless (matchDescriptive descriptiveLabel' descriptives)
    $ throwError noSeeMSG
    {-
    We do not test perceptibility here, just existence
  -- can player see it?
  unless (testPerceptability testableEntity)
    $ throwError noSeeMSG
    -}
  -- tests pass
  pure testableEntity
  where
    noSeeMSG = "You don't see that here."
-- (AdjNoun _ (Adjective adj) (Noun noun))
-}
{-
verifyAccessabilityNP :: NounPhrase -> GameStateExceptT (GID Object, Object)
verifyAccessabilityNP (NounPhrase1 _ (NounPhrase2 adj (Noun n))) =
  verifySimple (descriptiveLabel adj) (directObjectLabel n)
verifyAccessabilityNP (NounPhrase2 adj (Noun n)) =
  verifySimple (descriptiveLabel adj) (directObjectLabel n)
verifyAccessabilityNP _ = throwError "verifyAccessabilityAP unfinished"


-- verifyAccessabilityAP _ = throwError "verifyExistenceAP unfinished"

matchDescriptive :: Label Adjective -> [Label Adjective] -> Bool
matchDescriptive  testDescriptive descriptives =
  testDescriptive `elem` descriptives
-}
{-
data Orientation 
  = ContainedBy' ContainedBy 
  | Inventory 
  | Floor (GID Object)
  | AnchoredTo' (GID Object, Proximity) 
  | Anchoring RoomSection
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
{-
verifyAccessabilityAPNP :: AdjPhrase
                            -> NounPhrase
                            -> GameStateExceptT (GID Object, Object)
verifyAccessabilityAPNP (Adjective adj) (Noun noun) = do
  verifySimple (Label adj) (Label noun)
verifyAccessabilityAPNP _ _ = throwError "verifyAccessabilityAPNP unfinished"
-- (PrepPhrase1 _ (Noun noun))
-}
verifyAccessabilityPP :: (Imperative -> GameStateExceptT ())
                          -> PrepPhrase
                          -> ResultAccessabilityPP
verifyAccessabilityPP clarifierM (PrepPhrase1 _ (Noun noun)) = pure (Left pass) {- do
  (LabelToGIDListMapping m) <- _objectLabelMap'
                                <$> (getLocationM =<< getLocationIdM)
  entities <- throwMaybeM nopeErr
            --  =<< capturePerceptiblesM
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
    -}
verifyAccessabilityPP clarifierM (PrepPhrase1 _ (NounPhrase2 adj (Noun noun))) = pure $ Left pass {- do
  (LabelToGIDListMapping m) <- _objectLabelMap'
                                <$> (getLocationM =<< getLocationIdM)
  entities <- throwMaybeM nopeErr
            --  =<< capturePerceptiblesM
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
    -}
verifyAccessabilityPP _ _ =
  throwError "verifyAccessabilityPP: evaluate not completed"

matchAdjective :: Label Adjective -> Object -> Bool
matchAdjective label (Object{..}) = label `elem` _descriptives'

verifySensibilityNPPP :: (Imperative -> GameStateExceptT ())
                          -> NounPhrase
                          -> PrepPhrase
                          -> GameStateExceptT () -- (GID Object, Object)
verifySensibilityNPPP clarifyingM np pp = pass {- do
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
-}