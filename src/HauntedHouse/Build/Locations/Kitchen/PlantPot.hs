module HauntedHouse.Build.Locations.Kitchen.PlantPot where 
import HauntedHouse.Game.Model.World 
import HauntedHouse.Build.ObjectTemplate 
import HauntedHouse.Game.Model.Mapping (GIDToDataMapping (..))
import qualified Data.Map.Strict
import HauntedHouse.Game.Model.Condition (Moveability(..), Perceptibility (..))

buildPlantPot :: GameStateExceptT ()
buildPlantPot = do
  world <- _world' <$> get
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = 
        GIDToDataMapping $ Data.Map.Strict.insert plantPotGID plantPot 
          $ (_unGIDToDataMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}})  

plantPot :: Object 
plantPot = Object {
      _shortName'      = "A plant pot"
    , _odescription'   = [desc]
    , _descriptives'   = []
    , _moveability'    = NotMoveable
    , _perceptability' = Perceptible
    , _orientation'    = orientation 
    , _mNexus'         = Nothing
  }
  where 
    desc = "You can plant plants in the plant pot."


orientation :: Orientation 
orientation = ContainedBy' $ ContainedBy {
    _containedBy' = On kitchenShelfGID
  , _self = plantPotGID
}
{-
potNexus :: Nexus 
potNexus = (Nexus . Left) potContainment

potContainment :: Containment 
potContainment = (Containment . That . Right) potContainedBy 

potContainedBy :: ContainedBy 
potContainedBy = ContainedBy {
    _containedBy'     = Right kitchenShelfGID
  , _objectContained' = plantPotGID
} 
-}