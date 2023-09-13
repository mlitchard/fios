module HauntedHouse.Build.Locations.Kitchen.PlantPot where 
import HauntedHouse.Game.Model.World 
import HauntedHouse.Build.ObjectTemplate 
import HauntedHouse.Game.Model.Mapping (GIDToDataMapping (..), ContainerMap (..), Label (..))
import qualified Data.Map.Strict
import HauntedHouse.Game.Model.Condition (Moveability(..), Perceptibility (..))
import Data.These (These(..))
import HauntedHouse.Game.Actions.Get (standardGetM)
import HauntedHouse.Tokenizer (Lexeme(PLANT, SMALL))

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
      _shortName'      = "plant pot"
    , _odescription'   = [desc]
    , _descriptives'   = [Label PLANT, Label SMALL]
    , _moveability'    = NotMoveable
    , _perceptability' = Imperceptible
    , _orientation'    = orientation 
    , _mNexus'         = Nothing
    , _standardActions' = standardActions
  }
  where 
    desc = "You can plant plants in the plant pot."

standardActions :: StandardActions
standardActions = StandardActions 
  { _get' = standardGetM plantPotGID
  , _put' = pass 
  }
orientation :: Orientation 
orientation = ContainedBy' $ ContainedBy {
    _containedBy' = On kitchenShelfGID
  , _self = plantPotGID
}

potNexus :: Nexus 
potNexus = Containment' potContainment

potContainment :: Containment 
potContainment = (Containment . This) potContainedIn  

potContainedIn :: ContainedIn 
potContainedIn = ContainedIn 
  { _containerInterface' = potInterface 
  , _containedIn' = ContainerMap mempty
  }

potInterface :: ContainerInterface 
potInterface =  ContainerInterface {
    _describe' = mempty
  , _openState' = Open
  , _openAction' = pass 
  , _closeAction' = pass 
  , _lockAction' = pass
  , _unlockAction' = pass 
} 
