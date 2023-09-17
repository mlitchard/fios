{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use one" #-}
module HauntedHouse.Game.World where

import HauntedHouse.Game.Object
import qualified Data.Map.Strict
  (delete,insert, lookup, Map, singleton, adjust)
import HauntedHouse.Game.Model.Mapping
        (GIDToDataMapping(GIDToDataMapping, _unGIDToDataMapping'),
          LabelToGIDListMapping(..),Label (..), LabelToGIDMapping (..), GIDList)
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.World
import Control.Monad.Except (MonadError(..))
import HauntedHouse.Recognizer (NounPhrase, PrepPhrase)
import qualified Data.List.NonEmpty
import Data.These

setWorldExitMapM :: GID Exit -> Exit -> GameStateExceptT ()
setWorldExitMapM gid exit = do
  world <- _world' <$> get
  let updated = GIDToDataMapping $ Data.Map.Strict.insert gid exit
                  $ (_unGIDToDataMapping' . _exitMap') world
  modify' (\gs -> gs {_world' = world {_exitMap' = updated}})

setLocationMapM :: GID Location -> Location -> GameStateExceptT ()
setLocationMapM gid location = do
  world <- _world' <$> get
  let updated = GIDToDataMapping $ Data.Map.Strict.insert gid location
                  $ (_unGIDToDataMapping' . _locationMap') world
  modify' (\gs -> gs {_world' = world {_locationMap' = updated}})

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
  nexus <- throwMaybeM nexusErr . _mNexus' =<< getObjectM gid
  case nexus of
    Portal' (Portal _ exitGID) -> getExitM exitGID
    _ -> throwError notExitMsg
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

findAnchoredTo :: (GID Object, Object) -> Maybe FoundAnchoredTo
findAnchoredTo object = case object of
  (gid,obj@(Object _ _ _ _ _ _  (AnchoredTo' gp) _ _)) -> Just $ FoundAnchoredTo
                                                                (gid, obj) gp
  _ -> Nothing

getContainment :: Nexus -> Maybe Containment
getContainment (Containment' containment) = Just containment
getContainment _ = Nothing
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

removeFromContainmentM :: GID Object
                          -> OnOrIn
                          -> Containment
                          -> GameStateExceptT ()
removeFromContainmentM gid _ (Containment containment) = do
  entityLabel <- _entityLabel' <$> getObjectM gid
  case containment of
    This containedIn -> throwError "removeFromContainmentM: contradiction cIn"
    That containedOn -> pass
    These _ containedOn -> pass
  where
    cinMSG = "removeFromContainmentM: contradiction cIn"

-- updateContainerMap :: Label Object -> ContainerMap -> 