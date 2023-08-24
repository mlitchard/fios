{-# OPTIONS_GHC -Wno-orphans #-}
module HauntedHouse.Game.Object where

import qualified Data.Map.Strict (lookup, insert, null)
import HauntedHouse.Game.Model.Mapping
    ( GIDToDataMapping(GIDToDataMapping, _unGIDToDataMapping') )
import HauntedHouse.Game.Model.GID (GID (GID))
import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Model.Condition

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

data Object = Object
  { _shortName'     :: Text
  , _odescription'  :: Text
  , _conditions'    :: [Conditions]
  , _descriptives'  :: [Label Adjective]
  } deriving stock Show


instance VisibleObject (GID Object) where
  isVisible :: GID Object -> GameStateExceptT Bool
  isVisible gid = do
    conditions <- _metaConditions' <$> getObjectM gid
    pure $  any isPercieved conditions
    where
      isPercieved (MetaCondition (Perceptibility' x) _) = x == Perceptible
      isPercieved _ = False
-}