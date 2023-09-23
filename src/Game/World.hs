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
import qualified Data.List.NonEmpty
import qualified Data.List
import qualified Data.Text
import Data.List.NonEmpty

setWorldExitMapM :: GID Object -> GID Location -> GameStateExceptT ()
setWorldExitMapM oGid lGid = do
  world <- _world' <$> get
  let updated = GIDToGIDMapping $ Data.Map.Strict.insert oGid lGid
                  $ (_unGIDtoGIDMapping' . _exitMap') world
  modify' (\gs -> gs {_world' = world {_exitMap' = updated}})

setLocationMapM :: GID Location -> Location -> GameStateExceptT ()
setLocationMapM gid location = do
  world <- _world' <$> get
  let updated = GIDToDataMapping $ Data.Map.Strict.insert gid location
                  $ (_unGIDToDataMapping' . _locationMap') world
  modify' (\gs -> gs {_world' = world {_locationMap' = updated}})

initContainerMapM :: GID Object -> Container -> GameStateExceptT ()
initContainerMapM gid containment = do
  world <- _world' <$> get
  let updated = GIDToDataMapping $ Data.Map.Strict.insert gid containment
                  $ (_unGIDToDataMapping' . _containerMap') world
  modify' (\gs -> gs {_world' = world {_containerMap' = updated}})

setContainerMap :: GID Object -> GID Object -> GameStateExceptT ()
setContainerMap withContainerGid addedGid = do
  world <- _world' <$> get
  containerMap <- _unGIDToDataMapping' . _containerMap' . _world' <$> get
  label <- _entityLabel' <$> getObjectM addedGid
  containment <- throwMaybeM errMsg
                  $ Data.Map.Strict.lookup withContainerGid containerMap
  let updatedContainer = updateContainer addedGid label containment
      updatedContainerMap = GIDToDataMapping 
                              $ Data.Map.Strict.insert 
                                  withContainerGid 
                                  updatedContainer 
                                  containerMap
  modify' (\gs -> gs{_world' = world{_containerMap' = updatedContainerMap}})
  where
    errMsg = "setContainerMap: not a container " <> show withContainerGid
updateContainer :: GID Object -> Label Object -> Container -> Container
updateContainer addedGid label (Container (ContainerMap cmap)) =
  let singleList = Data.List.NonEmpty.singleton addedGid
      updated = Data.Map.Strict.insertWith (<>) label singleList cmap
  in Container (ContainerMap updated)
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
    unwrappedLocationMap = _unGIDToDataMapping' . _locationMap' . _world'
    unwrappedObjectMap = _unLabelToGIDListMapping' . _objectLabelMap'

setLocationM :: Location -> GameStateExceptT ()
setLocationM location = do
  lgid <- _playerLocation' . _player' <$> get
  world <- _world' <$> get
  locationMap <- GIDToDataMapping
                  . Data.Map.Strict.insert lgid location
                  . unwrappedMap <$> get
  modify' (\gs -> gs{_world' = world{_locationMap' = locationMap} })
    where
      unwrappedMap = _unGIDToDataMapping' . _locationMap' . _world'

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
  location@(Location _ _ _ _ _  mDirections) <- getLocationM locationGID
  let directionMap = case mDirections of
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

makeErrorReport :: NounPhrase -> PrepPhrase -> Text
makeErrorReport _np _pp = "You don't see that here"

findShelfM :: GID Object -> GameStateExceptT (Maybe Text)
findShelfM gid = do
  mContainer <- Data.Map.Strict.lookup gid
        . _unGIDToDataMapping'
        . _containerMap'
        . _world'
        <$> get
  case mContainer of
    Nothing -> pure Nothing
    Just (Container cmap) -> findContainedOnShortNameM cmap

findContainedOnShortNameM :: ContainerMap Object -> GameStateExceptT (Maybe Text)
findContainedOnShortNameM (ContainerMap cmap) = do
  objects <- mapM getObjectM
              $ concatMap Data.List.NonEmpty.toList
              $ Data.Map.Strict.elems cmap
  let res = mapMaybe findContainedOnShortName objects
  pure $ if Data.List.null res
          then Nothing
          else Just (Data.Text.concat res)

findContainedOnShortName :: Object -> Maybe Text
findContainedOnShortName (Object {..}) = case _orientation' of
  ContainedBy' (ContainedBy (On _) _) -> Just _shortName'
  _ -> Nothing

findAnchoredTo :: (GID Object, Object) -> Maybe FoundAnchoredTo
findAnchoredTo object = case object of
  (gid,obj@(Object _ _ _ _ _ _ (AnchoredTo' gp) _)) -> Just $ FoundAnchoredTo
                                                                (gid, obj) gp
  _ -> Nothing

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