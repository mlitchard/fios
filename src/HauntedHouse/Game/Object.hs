module HauntedHouse.Game.Object where

import qualified Data.Map.Strict (lookup, insert)

import HauntedHouse.Internal ( throwMaybeM )
import HauntedHouse.Game.Model.Mapping
    ( GIDToDataMapping(GIDToDataMapping, _unGIDToDataMapping') )
import HauntedHouse.Game.Model.GID (GID (GID))
import HauntedHouse.Game.Model (GameState (..), GameStateExceptT)
import HauntedHouse.Game.Model.World 
        (World (..), Object (..), ContainedIn, ContainedOn)
import HauntedHouse.Game.Narration.Containers (displayContainer)

displayObjectM :: GID Object -> GameStateExceptT ()
displayObjectM gidObject = do
  (Object shortname _ mContainment _ _ ) <- getObjectM gidObject
  print shortname
  whenJust mContainment (`whenLeft_` displayContainer)
  pass

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
  pass