{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use one" #-}
module Game.World where

import Game.Object
import qualified Data.Map.Strict
  (delete,insert, lookup, Map, singleton, elems, insertWith)
import Game.Model.Mapping
import Game.Model.GID (GID)
import Game.Model.World
import Recognizer (NounPhrase, PrepPhrase)
import qualified Data.List
import qualified Data.Text
import qualified Data.List.NonEmpty
-- import Data.List.NonEmpty

setWorldExitMapM :: GID Object -> GID Location -> GameStateExceptT ()
setWorldExitMapM oGid lGid = do
  world <- _world' <$> get
  let updated = GIDToGIDMapping $ Data.Map.Strict.insert oGid lGid
                  $ (_unGIDtoGIDMapping' . _exitMap') world
  modify' (\gs -> gs {_world' = world {_exitMap' = updated}})

setLocationMapM :: GID Location -> Location -> GameStateExceptT ()
setLocationMapM gid location = do
  world <- _world' <$> get
  let updated = GIDToDataMap $ Data.Map.Strict.insert gid location
                  $ (_unGIDToDataMap' . _locationMap') world
  modify' (\gs -> gs {_world' = world {_locationMap' = updated}})

initContainerMapM :: GID Object -> Container -> GameStateExceptT ()
initContainerMapM gid containment = do
  world <- _world' <$> get
  let updated = GIDToDataMap $ Data.Map.Strict.insert gid containment
                  $ (_unGIDToDataMap' . _containerMap') world
  modify' (\gs -> gs {_world' = world {_containerMap' = updated}})

setContainerMap :: GID Object -> ContainedEntity -> GameStateExceptT ()
setContainerMap withContainerGid containedEntity@(ContainedEntity {..}) = do
  world <- _world' <$> get
  containerMap <- _unGIDToDataMap' . _containerMap' . _world' <$> get
  label <- _entityLabel' <$> getObjectM _containedGid'
  containment <- throwMaybeM errMsg
                  $ Data.Map.Strict.lookup withContainerGid containerMap
  let updatedContainer = updateContainer containedEntity label containment
      updatedContainerMap = GIDToDataMap
                              $ Data.Map.Strict.insert
                                  withContainerGid
                                  updatedContainer
                                  containerMap
  modify' (\gs -> gs{_world' = world{_containerMap' = updatedContainerMap}})
  where
    errMsg = "setContainerMap: not a container " <> show withContainerGid

updateContainer :: ContainedEntity -> Label Object -> Container -> Container
updateContainer containedEntity label (Container cmap) =
  let singleList = Data.List.NonEmpty.singleton containedEntity
      updated = Data.Map.Strict.insertWith (<>) label singleList cmap
  in Container updated

getGIDListM :: GID Location
                -> Label Object
                -> GameStateExceptT (NonEmpty (GID Object))
getGIDListM locationGID label =
  throwMaybeM labelErr . lookupLabel
    =<< throwMaybeM locationErr . lookupGID
    =<< get
  where
    locationErr = "location does not exist " <> show locationGID
    labelErr = "Could not find that gid list based on label " <> show label
    lookupLabel = Data.Map.Strict.lookup label <$> unwrappedObjectMap
    lookupGID = Data.Map.Strict.lookup locationGID <$> unwrappedLocationMap
    unwrappedLocationMap = _unGIDToDataMap' . _locationMap' . _world'
    unwrappedObjectMap = _unLabelToGIDListMapping' . _objectLabelMap'

setLocationM :: Location -> GameStateExceptT ()
setLocationM location = do
  lgid <- _playerLocation' . _player' <$> get
  world <- _world' <$> get
  locationMap <- GIDToDataMap
                  . Data.Map.Strict.insert lgid location
                  . unwrappedMap <$> get
  modify' (\gs -> gs{_world' = world{_locationMap' = locationMap} })
    where
      unwrappedMap = _unGIDToDataMap' . _locationMap' . _world'

getLocationInventoryM :: GID Location
                          -> GameStateExceptT
                              (Data.Map.Strict.Map (Label Object) (GIDList Object))
getLocationInventoryM  gid =
   _unLabelToGIDListMapping' . _objectLabelMap' <$> getLocationM gid

setLocationDirectionM :: GID Location
                          -> Label Exit
                          -> GID Object
                          -> GameStateExceptT ()
setLocationDirectionM locationGID exitLabel objectGID = do
  location@(Location {..}) <- getLocationM locationGID
  let directionMap = case _directions' of
        (Just (ExitGIDMap (LabelToGIDMapping directions))) -> updateDirections
                                                               directions
        Nothing -> Data.Map.Strict.singleton exitLabel objectGID
      directionMap' = toExitMap directionMap
      updatedLocation = location{_directions' = Just directionMap'}
  setLocationM updatedLocation
  where
    updateDirections :: Map (Label Exit) (GID Object)
                          -> Map (Label Exit) (GID Object)
    updateDirections = Data.Map.Strict.insert exitLabel objectGID

    toExitMap :: Map (Label Exit) (GID Object) -> ExitGIDMap
    toExitMap = ExitGIDMap . LabelToGIDMapping

containedBy :: GID Object -- object contained
                -> GID Object -- object containing
                -> GameStateExceptT ContainedPlacement
containedBy containedGID containerGID = do
  (GIDToDataMap cmap ) <- _containerMap' . _world' <$> get
 
  containedXS <- makeContainedXS 
                  <$> throwMaybeM notContainer 
                  (Data.Map.Strict.lookup containerGID cmap)
  
  (ContainedEntity {..}) <- throwMaybeM notContained $ find matchContained containedXS
  pure _placement'
  where
    concatElems = concat . (Data.List.NonEmpty.toList <<$>> Data.Map.Strict.elems)
    makeContainedXS = concatElems . _unContainer'
    matchContained (ContainedEntity {..}) = _containedGid' == containedGID
    notContained = "containedBy error: "
                      <> show containedGID
                      <> "is not contained by "
                      <> show containerGID
    notContainer = "containedBy error: "
                      <> show containerGID
                      <> "not a container"
-- _anchoredObjects'

makeErrorReport :: NounPhrase -> PrepPhrase -> Text
makeErrorReport _np _pp = "You don't see that here"

-- assumes if the Label exists, the gid is in it's list
-- used when player leaves location
removeEntityLabelMapM :: Label Object -> GID Object  -> GameStateExceptT ()
removeEntityLabelMapM entityLabel gid = do
  lid <- getLocationIdM
  location <- getLocationM lid
  -- adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
  let labelMap = (_unLabelToGIDListMapping' . _objectLabelMap') location
  gidList :: GIDList Object <- throwMaybeM lFailMSG
              $ Data.Map.Strict.lookup entityLabel labelMap

  let mUpdatedLabelMap :: Maybe (LabelToGIDListMapping Object Object)
      mUpdatedLabelMap = removeGID gidList labelMap
      updatedLabelMap = case mUpdatedLabelMap of
        Just updatedLabelMap' -> updatedLabelMap'
        Nothing              -> LabelToGIDListMapping
                                  $ Data.Map.Strict.delete entityLabel labelMap
  setLocationMapM lid (location {_objectLabelMap' = updatedLabelMap})
  where
    lFailMSG = "removeEntityLabelMapM: That label doesn't exit"

    removeGID gidList labelMap = do
      filteredList <- nonEmpty $ Data.List.NonEmpty.filter (/= gid) gidList
      pure
        $ LabelToGIDListMapping
        $ Data.Map.Strict.insert entityLabel filteredList labelMap