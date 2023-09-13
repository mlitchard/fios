{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use one" #-}
module HauntedHouse.Build.Locations.Kitchen.Floor where 

import HauntedHouse.Game.Model.World 
import HauntedHouse.Build.ObjectTemplate (plantPotGID, kitchenFloorGID)
import HauntedHouse.Game.Model.Mapping 
import qualified Data.Map.Strict 
import qualified Data.List.NonEmpty
import Data.These
import HauntedHouse.Game.Model.Condition (Moveability(..), Perceptibility (..))
import HauntedHouse.Build.ObjectLabels (floorLabel)
-- Anchoring RoomAnchor

buildKitchenFloor :: GameStateExceptT ()
buildKitchenFloor = do 
  world <- _world' <$> get
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = 
        GIDToDataMapping $ Data.Map.Strict.insert kitchenFloorGID buildFloor 
          $ (_unGIDToDataMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}}) 

buildFloor :: Object 
buildFloor = Object 
  { _shortName'         = "kitchen floor."
    , _odescription'    = [desc]
    , _descriptives'     = [] 
    , _moveability'     = NotMoveable
    , _perceptability'  = Perceptible
    , _orientation'     = orientation
    , _mNexus'          = (Just . Containment') floorContainer
    , _standardActions' = standardActions
  }
  where
    desc = "A non-descipt tiled kitchen floor."
    orientation = Anchoring CenterAnchor

floorContainer :: Containment 
floorContainer = (Containment . Data.These.That) containedOn 

containedOn :: ContainedOn 
containedOn = 
  (ContainedOn . ContainerMap) $ Data.Map.Strict.singleton floorLabel floorInv

floorInv :: GIDList Object
floorInv = Data.List.NonEmpty.singleton plantPotGID 

standardActions :: StandardActions
standardActions = StandardActions 
  { _get' = const pass 
  , _put' = const pass 
  }