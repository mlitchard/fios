module HauntedHouse.Build.Locations.Kitchen.PlantPot where 
import HauntedHouse.Game.Model.World 
import HauntedHouse.Build.ObjectTemplate 
import HauntedHouse.Game.Model.Mapping (GIDToDataMapping (..), ContainerMap (..), Label (..))
import qualified Data.Map.Strict
import HauntedHouse.Game.Model.Condition (Moveability(..), Perceptibility (..))
import Data.These (These(..))
import HauntedHouse.Game.Actions.Get (tryGetM)
import HauntedHouse.Tokenizer (Lexeme(PLANT, SMALL, POT))
import HauntedHouse.Game.Actions.Look

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
    , _entityLabel' = Label POT 
    , _odescription'   = [desc]
    , _descriptives'   = [Label PLANT, Label SMALL]
    , _moveability'    = NotMoveable
    , _perceptability' = Perceptible
    , _orientation'    = floorOrientation -- orientation 
    , _mNexus'         = Nothing -- ToDo must be a container
    , _standardActions' = standardActions
  }
  where 
    desc = "You can plant plants in the plant pot."

standardActions :: StandardActions
standardActions = StandardActions 
  { _get' = tryGetM plantPotGID
  , _put' = const pass 
  , _lookIn' = const (print ("plant pot gets special lookin" :: Text))
  , _lookAt' = lookAt
  , _lookOn' = const (print ("plant pot gets special lookon" :: Text)) -- ToDo
  }
orientation :: Orientation 
orientation = ContainedBy' $ ContainedBy {
    _containedBy' = On kitchenShelfGID
  , _self = plantPotGID
}

floorOrientation :: Orientation 
floorOrientation = Floor kitchenFloorGID

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
