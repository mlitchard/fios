{-# OPTIONS_GHC -Wno-orphans #-}
module HauntedHouse.Game.Object where

import qualified Data.Map.Strict (lookup, insert, insertWith)
import HauntedHouse.Game.Model.Mapping

import HauntedHouse.Game.Model.GID (GID (GID))
import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Model.Condition
import qualified Data.List.NonEmpty
import Data.These
import Control.Monad.Except (MonadError(..))

getObjectM :: GID Object -> GameStateExceptT Object
getObjectM gid@(GID gid') = do
  throwMaybeM objErr . (Data.Map.Strict.lookup gid <$> unwrappedMap) =<< get
  where
    objErr = toText $ ("Could not find object with gid " :: String) <> show gid'
    unwrappedMap = _unGIDToDataMapping' . _objectMap' . _world'

setObjectMapM :: GID Object -> Object -> GameStateExceptT ()
setObjectMapM gid object = do
  world <- _world' <$> get
  let objectMap = _objectMap' world
      gidToDataMap  = GIDToDataMapping
                          . Data.Map.Strict.insert gid object
                          $ _unGIDToDataMapping' objectMap
  modify' (\gs -> gs {_world' = world {_objectMap' = gidToDataMap }})

setObjectLabelMapM :: GID Location
                        -> Label Object
                        -> GID Object
                        -> GameStateExceptT ()
setObjectLabelMapM locationGID objectLabel objectGID = do
  location@(Location _ _ _ _ _ (LabelToGIDListMapping objectLabelMap) _)
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

capturePerceptibleM :: GIDList Object
                        -> GameStateExceptT (Maybe (NonEmpty (GID Object,Object)))
capturePerceptibleM objectList = do
  nonEmpty . mfilter (\(_, obj) -> isPercieved obj)
    <$> mapM getObjectGIDPairM (Data.List.NonEmpty.toList objectList)

getContainerInterfaceM :: Object -> GameStateExceptT ContainerInterface
getContainerInterfaceM entity = do
  nexus <- throwMaybeM notContainerMSG (_mNexus' entity)
  containment <- case nexus of 
                  Containment' containment -> pure containment 
                  _ -> throwError notContainerMSG 
  throwMaybeM notContainedInMSG $ case _unContainment' containment of
    (This (ContainedIn interface _)) -> Just interface
                                      
    (That _) -> Nothing -- throwError notContainedInMSG
    (These (ContainedIn interface _) _) -> Just interface
                                            
  where
    notContainedInMSG = "getContainerInterface error: can't put things in this"
    notContainerMSG = "getContainerInterface error: "
                        <> "called on an entity that isn't a container."

isPercieved :: Object -> Bool
isPercieved (Object _ _ _ _ Perceptible _ _ _) = True
isPercieved _ = False