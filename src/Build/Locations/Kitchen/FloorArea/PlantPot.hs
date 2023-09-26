module Build.Locations.Kitchen.FloorArea.PlantPot where
import Game.Model.Mapping
        (GIDToDataMapping (..), Label (..))
import qualified Data.Map.Strict
import Game.Model.Condition (Moveability(..), Perceptibility (..))
import Tokenizer.Data ( Lexeme(PLANT, SMALL, POT) )
import Game.Model.World
import Game.Model.Display (updateEnvironmentM)
import Build.ObjectTemplate (plantPotGID, kitchenFloorGID)
import Build.Locations.Kitchen.FloorArea.Actions.PlantPot.Get (getAction)
import Build.Locations.Kitchen.FloorArea.Actions.PlantPot.NoCanDo
        (unlockAction, goAction, openAction, closeAction, lockAction)
import Build.Locations.Kitchen.FloorArea.Actions.PlantPot.Put (putAction)
import Build.Locations.Kitchen.FloorArea.Actions.PlantPot.Look (emptyPlantPotLookAction)

buildPlantPot :: GameStateExceptT ()
buildPlantPot = do
  world <- _world' <$> get
  let objectMap' :: GIDToDataMapping Object Object
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
    , _standardActions' = standardActions
  }
  where
    desc = "You can plant plants in the plant pot."

standardActions :: StandardActions
standardActions = StandardActions {
    _getAction' = getAction
  , _putAction' = putAction
  , _lookAction' = emptyPlantPotLookAction
  , _openAction' = openAction
  , _closeAction' = closeAction
  , _lockAction' = lockAction
  , _unlockAction' = unlockAction
  , _goAction' = goAction
}

floorOrientation :: Orientation
floorOrientation = ContainedBy' containedBy

containedBy :: ContainedBy 
containedBy = ContainedBy {
    _containedBy' = On kitchenFloorGID
  , _self' = plantPotGID
}
