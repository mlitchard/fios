{-# OPTIONS_GHC -Wno-orphans #-}
module Game.Object where

import qualified Data.Map.Strict (lookup, insert, insertWith, elems, toList)
import Game.Model.Mapping
import Game.Model.GID (GID (GID))
import Game.Model.World
import qualified Data.List.NonEmpty
import Control.Monad.Except (MonadError(throwError))
import Game.Model.Condition (Proximity)

getContainedPlacement :: GID Object -- contained gid 
                          -> GID Object -- container gid 
                          -> GameStateExceptT (GID Object,ContainedPlacement)
getContainedPlacement containedGid containerGid = do
  containerMap <- _unGIDToDataMap' . _containerMap' . _world' <$> get
  (Container container) <- throwMaybeM notContainerMsg
                            $ Data.Map.Strict.lookup containedGid containerMap
  (Object {..}) <- getObjectM containedGid
  containedEntities <- throwMaybeM (noEntities _entityLabel')
                        $ Data.Map.Strict.lookup _entityLabel' container
  (ContainedEntity {..}) <- throwMaybeM notFoundMsg
                    $ find (isContainedGid containedGid) $ Data.List.NonEmpty.toList containedEntities
  pure $ (,) containerGid _placement'
  where
    noEntities (Label label) = "getContainedPlacement error: when looking for "
                                  <> show containedGid
                                  <> " could not find any "
                                  <> show label
    notFoundMsg = "getContainedPlacement error: "
                    <> show containerGid
                    <> " not found."
    notContainerMsg = "getContainedPlacement error : "
                        <> show containerGid <> " is not a container."

isContainedGid :: GID Object -> ContainedEntity -> Bool
isContainedGid gid (ContainedEntity gid' _) = gid == gid'

getAnchoreds :: GID Location
                -> RoomSection
                -> GID Object
                -> Text
                -> GameStateExceptT (Maybe (NonEmpty Anchored))
getAnchoreds lid roomSection anchorGid roomSectionMsg = do
  (Location {..}) <- getLocationM lid
  objectAnchors <- _unObjectAnchors'
                      <$> throwMaybeM roomSectionMsg
                      (Data.Map.Strict.lookup roomSection _anchoredObjects')
  throwMaybeM notAnchorMsg
                    $ Data.Map.Strict.lookup anchorGid objectAnchors
  where
    notAnchorMsg = show anchorGid <> "is not an anchor in " <> show roomSection

getAnchoredTo :: GID Location -> GID Object -> GameStateExceptT AnchoredTo
getAnchoredTo lid anchoredGid = do
  (Location {..}) <- getLocationM lid
  let anchors = concatMap (Data.Map.Strict.toList . _unObjectAnchors')
            $ Data.Map.Strict.elems _anchoredObjects'
  case findAnchoredTo anchoredGid anchors of
    Nothing -> throwError programmerErr
    Just anchoredTo -> pure anchoredTo
  where
    programmerErr = "getAnchoredTo says programmer error: "
                      <> (show anchoredGid <> " should be anchored but isn't.")


findAnchoredTo :: GID Object
                    -> [(GID Object, Maybe (NonEmpty Anchored))]
                    -> Maybe AnchoredTo
findAnchoredTo _ [] = Nothing
findAnchoredTo _ [(_,Nothing)] = Nothing
findAnchoredTo findAnchoredGid [(anchorGid, Just anchoredXS)] =
  findAnchoredTo' findAnchoredGid anchorGid anchoredXS
findAnchoredTo findAnchoredGid ((_,Nothing):xs) = findAnchoredTo findAnchoredGid xs
findAnchoredTo findAnchoredGid ((anchorGid, Just anchoredXS):xs) =
  case findAnchoredTo' findAnchoredGid anchorGid anchoredXS of
    (Just anchoredTo) -> Just anchoredTo
    Nothing -> findAnchoredTo findAnchoredGid xs


findAnchoredTo' :: GID Object
                    -> GID Object
                    -> NonEmpty Anchored
                    -> Maybe AnchoredTo
findAnchoredTo' findAnchoredGid anchorGid (Anchored testGid proximity :| [])
  | findAnchoredGid == testGid = Just (AnchoredTo anchorGid proximity)
  | otherwise = Nothing

findAnchoredTo' findAnchoredGid anchorGid (Anchored testGid proximity :| (x:xs))
  | findAnchoredGid == testGid = Just (AnchoredTo anchorGid proximity)
  | otherwise = findAnchoredTo' findAnchoredGid anchorGid (x :| xs)
{-
findAnchoredTo anchoredGid [(anchorGid,xs)] = findAnchoredTo' anchoredGid xs
findAnchoredTo anchoredGid ((anchorGid,anchoredXS):xs) =
  case findAnchoredTo' anchoredGid anchoredXS of
    Just anchoredTo -> Just anchoredTo
    Nothing -> findAnchoredTo anchoredGid xs

findAnchoredTo' :: GID Object -> [Anchored] -> Maybe AnchoredTo
findAnchoredTo' _ [] = Nothing
findAnchoredTo' anchoredGid ((Anchored testAnchored proximity):xs)
  | testAnchored == anchoredGid = Just (AnchoredTo anchoredGid proximity)
  | otherwise = findAnchoredTo' anchoredGid xs
-}

getObjectM :: GID Object -> GameStateExceptT Object
getObjectM gid@(GID gid') = do
  throwMaybeM objErr . (Data.Map.Strict.lookup gid <$> unwrappedMap) =<< get
  where
    objErr = toText $ ("Could not find object with gid " :: String) <> show gid'
    unwrappedMap = _unGIDToDataMap' . _objectMap' . _world'

setObjectMapM :: GID Object -> Object -> GameStateExceptT ()
setObjectMapM gid object = do
  world <- _world' <$> get
  let objectMap = _objectMap' world
      gidToDataMap  = GIDToDataMap
                          . Data.Map.Strict.insert gid object
                          $ _unGIDToDataMap' objectMap
  modify' (\gs -> gs {_world' = world {_objectMap' = gidToDataMap }})

setObjectLabelMapM :: GID Location
                        -> Label Object
                        -> GID Object
                        -> GameStateExceptT ()
setObjectLabelMapM locationGID objectLabel objectGID = do
  location@(Location _ _ _ (LabelToGIDListMapping objectLabelMap) _)
    <- getLocationM locationGID
  let updatedMap = LabelToGIDListMapping $ insertGID objectLabelMap
      updatedLocation = location{_objectLabelMap' = updatedMap}
  updateLocationM locationGID updatedLocation
  where
    singleList = Data.List.NonEmpty.singleton objectGID
    insertGID = Data.Map.Strict.insertWith (<>) objectLabel singleList

getObjectsFromLabelM :: Label Object
                        -> GameStateExceptT (NonEmpty (GID Object,Object))
getObjectsFromLabelM objectLabel = do
  objectGIDList <- getObjectGIDsFromLabelM objectLabel
  mapM getObjectGIDPairM objectGIDList

getObjectGIDPairM :: GID Object -> GameStateExceptT (GID Object, Object)
getObjectGIDPairM gid = do
  object <- getObjectM gid
  pure (gid, object)

getObjectGIDsFromLabelM :: Label Object
                            -> GameStateExceptT (NonEmpty (GID Object))
getObjectGIDsFromLabelM objectLabel@(Label obj) = do
  objectLabelMap <- unwrapMap <$> (getLocationM =<< getLocationIdM)
  throwMaybeM notFound $ Data.Map.Strict.lookup objectLabel objectLabelMap
  where
    unwrapMap = _unLabelToGIDListMapping' . _objectLabelMap'
    notFound = toText obj <> " not found"

namedDirectionM :: (Label Exit, GID Object) -> GameStateExceptT (Text,Label Exit)
namedDirectionM (label, gid) = do
  shortName <- _shortName' <$> getObjectM gid
  pure (shortName, label)

getShortNameM :: GID Object -> GameStateExceptT Text
getShortNameM gid = _shortName' <$> getObjectM gid

