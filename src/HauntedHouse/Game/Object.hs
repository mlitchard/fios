{-# OPTIONS_GHC -Wno-orphans #-}
module HauntedHouse.Game.Object where

import qualified Data.Map.Strict (lookup, insert, null, insertWith, singleton, Map)
import HauntedHouse.Game.Model.Mapping
    ( GIDToDataMapping(GIDToDataMapping, _unGIDToDataMapping'), Label, LabelToGIDListMapping (..), LabelToGIDMapping (..) )
import HauntedHouse.Game.Model.GID (GID (GID))
import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Model.Condition
import qualified Data.List.NonEmpty

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


{-
data Location = Location {
  _title'             :: Text
  , _description'     :: Text
  , _anchoredObjects' :: RoomAnchors
  , _anchoredTo'      :: AnchoredTo
  , _floorInventory'  :: Maybe (Data.List.NonEmpty.NonEmpty (GID Object))
  , _objectLabelMap'  :: LabelToGIDListMapping Object Object
  , _directions'      :: Maybe ExitGIDMap
}
type GIDList a = (Data.List.NonEmpty.NonEmpty (GID a))
insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a

data World = World
  { _objectMap'         :: GIDToDataMapping Object
  , _locationMap'       :: GIDToDataMapping Location
  , _descriptiveMap'    :: LabelToGIDListMapping Adjective Object
  , _exitMap'           :: GIDToDataMapping Exit
  }
-}
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

namedDirection :: (Label Exit, GID Object) -> GameStateExceptT (Text,Label Exit)
namedDirection (label, gid) = do
  shortName <- _shortName' <$> getObjectM gid
  pure (shortName, label)