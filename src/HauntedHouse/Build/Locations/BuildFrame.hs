module HauntedHouse.Build.Locations.BuildFrame where

import qualified Data.Map.Strict

import HauntedHouse.Game.Model.Mapping
    ( GIDToDataMapping(GIDToDataMapping, _unGIDToDataMapping'), LabelToGIDListMapping (..), GIDList)
    
import HauntedHouse.Game.Model.GID (GID)

import HauntedHouse.Game.Model.World 
        
import qualified Data.List.NonEmpty
import HauntedHouse.Recognizer (Adjective)

buildLocationMap :: GID Location -> Location -> GameStateExceptT ()
buildLocationMap locationGID location = pass {-  do
  world :: World <- _world' <$> get
  let locationMap' = unLocationMap world  
      updatedMap = GIDToDataMapping
                    $ Data.Map.Strict.insert
                        locationGID location locationMap'
  modify' (\gs -> gs {_world' = world {_locationMap' = updatedMap}})
  where
    unLocationMap = _unGIDToDataMapping' . _locationMap'

buildDescriptorMap :: LabelToGIDListMapping Adjective Object 
                        -> GameStateExceptT ()
buildDescriptorMap (LabelToGIDListMapping descriptorMap) = do 
  world <- _world' <$> get
  let (LabelToGIDListMapping descriptorMap') = _descriptiveMap' world 
      updatedMap = LabelToGIDListMapping
        $ Data.Map.Strict.unionWith updateMap descriptorMap descriptorMap'
  modify' (\gs -> gs {_world' = world{_descriptiveMap' = updatedMap}})
  where
    updateMap :: GIDList Object -> GIDList Object -> GIDList Object 
    updateMap xs ys = Data.List.NonEmpty.nub $ xs <> ys  
 
   -} 