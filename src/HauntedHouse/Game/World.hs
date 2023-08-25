{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use one" #-}
module HauntedHouse.Game.World where

import qualified Data.List.NonEmpty
import qualified Data.Map.Strict (insert, lookup, insertWith, Map, singleton)
import HauntedHouse.Game.Model.Mapping
        (GIDToDataMapping(GIDToDataMapping, _unGIDToDataMapping'),
          LabelToGIDListMapping(..),Label, LabelToGIDMapping (..), GIDList)
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.World

import HauntedHouse.Game.Object ( getObjectM )
import Control.Monad.Except (MonadError(..))

setWorldExitMapM :: GID Exit -> Exit -> GameStateExceptT ()
setWorldExitMapM gid exit = do
  world <- _world' <$> get
  let updated = GIDToDataMapping $ Data.Map.Strict.insert gid exit
                  $ (_unGIDToDataMapping' . _exitMap') world
  modify' (\gs -> gs {_world' = world {_exitMap' = updated}})

getExitM :: GID Exit -> GameStateExceptT Exit
getExitM gid = do
  throwLookup . _unGIDToDataMapping' . _exitMap' . _world' =<< get
  where
    throwLookup :: Data.Map.Strict.Map (GID Exit) Exit -> GameStateExceptT Exit
    throwLookup exitMap = throwMaybeM lookupErr $ Data.Map.Strict.lookup gid exitMap
    lookupErr = "Not found: exit gid " <> show gid

getExitObjectM :: Label Exit -> GameStateExceptT Exit
getExitObjectM exitLabel = do
  (ExitGIDMap (LabelToGIDMapping objGIDMap)) <- (throwMaybeM dirErr . _directions')
                                                  =<< (getLocationM =<< getLocationIdM)
  objGID <- throwMaybeM noExit $ Data.Map.Strict.lookup exitLabel objGIDMap
  getExitFromObjectM objGID
  where
      noExit = "Error: Exit does not exist " <> show exitLabel
      dirErr = "Error: No Exits"

getExitFromObjectM :: GID Object -> GameStateExceptT Exit
getExitFromObjectM gid = do
  (Nexus nexus) <- throwMaybeM nexusErr . _mNexus' =<< getObjectM gid
  case nexus of
    Left _ -> throwError notExitMsg
    Right (Portal _ exitGID) -> getExitM exitGID
  where
    nexusErr = "Not a nexus"
    notExitMsg = "Not an exit"

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
{-
setObjectLabelMapM :: Label Object -> GID Object -> GameStateExceptT ()
setObjectLabelMapM objectLabel objectGid = do
  location :: Location <-  getLocationM . _playerLocation' . _player' =<< get
  let unwrappedMap :: Data.Map.Strict.Map (Label Object) (GIDList Object)
      unwrappedMap = (_unLabelToGIDListMapping' ._objectLabelMap') location
      updatedMap = LabelToGIDListMapping
        $ Data.Map.Strict.insertWith (<>) objectLabel objectGid' unwrappedMap
      location' = location {_objectLabelMap' = updatedMap}
  setLocationM location'
  where
    objectGid' = Data.List.NonEmpty.singleton objectGid
-}
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

setLocationDirectionM :: GID Location 
                          -> Label Exit 
                          -> GID Object 
                          -> GameStateExceptT ()
setLocationDirectionM locationGID exitLabel objectGID = do 
  location@(Location _ _ _ _ _ _ mDirections) <- getLocationM locationGID 
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