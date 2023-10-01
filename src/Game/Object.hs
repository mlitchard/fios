{-# OPTIONS_GHC -Wno-orphans #-}
module Game.Object where

import qualified Data.Map.Strict (lookup, insert, insertWith)
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

getAnchored :: GID Location 
                -> RoomSection 
                -> GID Object
                -> Text
                -> GameStateExceptT (Maybe (NonEmpty Anchored))
getAnchored lid roomSection anchorGid roomSectionMsg = do 
  (Location {..}) <- getLocationM lid
  objectAnchors <- _unObjectAnchors'
                      <$> throwMaybeM roomSectionMsg
                      (Data.Map.Strict.lookup roomSection _anchoredObjects')
  throwMaybeM notAnchorMsg
                    $ Data.Map.Strict.lookup anchorGid objectAnchors  
  where 
    notAnchorMsg = show anchorGid <> "is not an anchor in " <> show roomSection

getProximity :: GID Location
                  -> RoomSection
                  -> GID Object
                  -> GID Object
                  -> GameStateExceptT Proximity
getProximity lid roomSection anchorGid anchoredGid = do
  maybeAnchored <- getAnchored lid roomSection anchorGid roomSectionMsg
  case maybeAnchored of
    Nothing -> throwError notAnchoredByMsg
    Just anchoreds -> case find isAnchored $ toList anchoreds of
                        Nothing -> throwError notAnchoredByMsg
                        Just (Anchored _ proximity) -> pure proximity
  where
    isAnchored (Anchored gid _) = gid == anchoredGid
    notAnchoredByMsg = show anchoredGid
                        <> " is not anchored by "
                        <> show anchorGid
    roomSectionMsg = show anchoredGid <> "not anchored in " <> show roomSection

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

