module Build.Locations.Kitchen.FloorArea.PlantPot where
import Game.Model.Mapping
        (Label (..))
import Tokenizer.Data ( Lexeme(PLANT, SMALL, POT) )
import Game.Model.World
import Build.ObjectTemplate (plantPotGID, kitchenFloorGID)
import Build.Locations.Kitchen.FloorArea.Actions.PlantPot.Get (getAction)
import Build.Locations.Kitchen.FloorArea.Actions.PlantPot.NoCanDo
        (unlockAction, goAction, openAction, closeAction, lockAction)
import Build.Locations.Kitchen.FloorArea.Actions.PlantPot.Put (putAction)
import Build.Locations.Kitchen.FloorArea.Actions.PlantPot.Look 
        (emptyPlantPotLookAction)
import Game.Object (setObjectMapM, getContainedPlacement)
import Game.Model.GID (GID)

buildPlantPot :: GameStateExceptT ()
buildPlantPot =  setObjectMapM plantPotGID plantPot

plantPot :: Object
plantPot = Object {
      _shortName'       = "plant pot"
    , _entityLabel'     = Label POT
    , _odescription'    = [desc]
    , _descriptives'    = [Label PLANT, Label SMALL]
    , _orientation'     = potOrientation getPotPlacement
    , _standardActions' = standardActions
  }
  where
    potOrientation = ContainedBy' 
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

getPotPlacement :: GameStateExceptT (GID Object,ContainedPlacement)
getPotPlacement = getContainedPlacement plantPotGID kitchenFloorGID 