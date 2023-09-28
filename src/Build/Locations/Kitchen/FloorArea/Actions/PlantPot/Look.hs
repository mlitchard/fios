module Build.Locations.Kitchen.FloorArea.Actions.PlantPot.Look where

import Game.Model.World
    ( LookOnF(LookOnF),
      LookInF(LookInF),
      LookAtF(LookAtF),
      LookFunctions(..),
      PerceptionFunctions(..),
      UpdatePerceptionFunctions(..),
      LookAction(..),
      GameStateExceptT,
      StandardActions (_lookAction'),
      Object(..) )
import Game.Model.Display (updateEnvironmentM)
import Game.Actions.Look.StandardLook (changeLookAction)
import Build.ObjectTemplate (plantPotGID)
import Game.Object (setObjectMapM)
import Game.Actions.Look.Update
        ( updateLookAtF, updateLookInF, updateLookOnF, updateOpenReport )

updateLookActionObject :: Object -> GameStateExceptT ()
updateLookActionObject = setObjectMapM plantPotGID

initialLookAction :: LookAction
initialLookAction = defaultLookAction

defaultLookAction :: LookAction
defaultLookAction = LookAction {
    _updatePerception' = defaultUpdatePerceptions
  , _perception' = defaultPerception
  , _lookFunctions' = defaultLookFunctions
}

defaultUpdatePerceptions :: UpdatePerceptionFunctions
defaultUpdatePerceptions = UpdatePerceptionFunctions {
    _updateOpenReport' = const pass
  , _updateVisibility' = const pass
}

defaultPerception :: PerceptionFunctions
defaultPerception = PerceptionFunctions {
    _lookPerceptionF' = id
  , _displayPerceptionF' = id
}

defaultLookFunctions :: LookFunctions
defaultLookFunctions = LookFunctions {
    _lookAt' = lookAtEmptyF
  , _lookIn' = lookInEmptyF
  , _lookOn' = lookOnF
}

lookAtHasPlantF :: LookAtF
lookAtHasPlantF = LookAtF lookHasPlant

lookInHasPlantF :: LookInF 
lookInHasPlantF = LookInF lookHasPlant 

lookOnF :: LookOnF 
lookOnF = LookOnF $ const (const (updateEnvironmentM msg)) 
  where 
    msg = "You see an etching that looks suspiciously like instructions"
            <> "Put soil in plant pot"
            <> "Plant pot plant in plant pot with trowel"
            <> "Alternative second step: Use trowel to plant pot plant in plant pot."

lookHasPlant :: a -> b -> GameStateExceptT ()
lookHasPlant = const (const (updateEnvironmentM msg))
  where
    msg = "Pot plant and plant pot have become one. "
            <> "You've completed this demo."
            <> "Magnetic Scrolls did it first."

lookInEmptyF :: LookInF 
lookInEmptyF = LookInF lookEmpty 

lookAtEmptyF :: LookAtF
lookAtEmptyF = LookAtF lookEmpty 

lookEmpty :: a -> b -> GameStateExceptT ()
lookEmpty = const (const (updateEnvironmentM msg))
  where 
    msg = "This empty pot plant could use some soil in it. "
            <> "To plant a plant,perhaps."

lookAtHasSoilF :: LookAtF 
lookAtHasSoilF = LookAtF lookHasSoil 

lookInHasSoilF :: LookInF 
lookInHasSoilF = LookInF lookHasSoil 

lookHasSoil :: a -> b -> GameStateExceptT ()
lookHasSoil = const (const (updateEnvironmentM msg))
  where
    msg = "The plant has soil and is now ready for a plant of some "
            <> "kind to be planted in it."

changeToHasSoilLook :: Object -> GameStateExceptT ()
changeToHasSoilLook entity@(Object {..}) =
  updateLookActionObject $ changeLookAction hasSoilLookAction' entity
  where
    hasSoilLookAction' =
      hasSoilLookAction (_standardActions'._lookAction')

emptyPlantPotLookAction :: LookAction 
emptyPlantPotLookAction = defaultLookAction 

hasSoilLookAction :: LookAction -> LookAction
hasSoilLookAction =
  updateOpenReport changeToPlantedLook
    . updateLookAtF lookAtHasSoilF
    . updateLookOnF lookOnF
    . updateLookInF lookInHasSoilF

hasPlantLookAction :: LookAction -> LookAction
hasPlantLookAction =
  updateOpenReport (const pass)
    . updateLookAtF lookAtHasPlantF
    . updateLookOnF lookOnF
    . updateLookInF lookInHasPlantF

changeToPlantedLook :: Object -> GameStateExceptT ()
changeToPlantedLook entity@(Object {..}) =
  updateLookActionObject $ changeLookAction hasPlantLookAction' entity
  where
    hasPlantLookAction' =
      hasPlantLookAction (_standardActions'._lookAction')
