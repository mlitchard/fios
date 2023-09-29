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
      Object(..), LookF, DisplayF )
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
    _updateBlockReport' = changeToNoSee
  , _updateDisplay' = changeToNoDisplay
}

defaultPerception :: PerceptionFunctions
defaultPerception = PerceptionFunctions {
    _lookPerceptionF' = canSeeF
  , _displayPerceptionF' = onDisplayF
}

onDisplayF :: DisplayF
onDisplayF = Just  

noDisplayF :: DisplayF
noDisplayF _ = Nothing

changeLookPerception :: (LookF -> LookF)
                          -> (Object -> GameStateExceptT ())
                          -> Object 
                          -> GameStateExceptT () 
changeLookPerception f g entity@(Object {..}) = do 
  updateLookActionObject entity {_standardActions' = standardActions 
            {_lookAction' = newUpdatePerception}}
  where 
    standardActions = _standardActions' 
    newUpdatePerception = 
      updateLookPerception f 
      $ updateBlockReport g (_standardActions'._lookAction')

changeDisplayPerception :: DisplayF
                              -> (Object -> GameStateExceptT ())
                              -> Object 
                              -> GameStateExceptT ()
changeDisplayPerception f g  entity@(Object {..}) = do 
  updateLookActionObject entity {_standardActions' = standardActions 
            {_lookAction' = newUpdatePerception}}
   where 
    standardActions = _standardActions' 
    newUpdatePerception = 
      updateDisplayPerception f 
      $ updateDisplay g (_standardActions'._lookAction')

changeToLookSee :: Object -> GameStateExceptT ()
changeToLookSee = changeLookPerception canSeeF changeToNoSee
   
changeToNoSee :: Object -> GameStateExceptT ()
changeToNoSee = changeLookPerception noSeeF changeToLookSee 
 
changeToCanDisplay :: Object -> GameStateExceptT ()
changeToCanDisplay = changeDisplayPerception onDisplayF changeToNoDisplay 

changeToNoDisplay :: Object -> GameStateExceptT () 
changeToNoDisplay = changeDisplayPerception noDisplayF changeToCanDisplay

updateDisplay :: (Object -> GameStateExceptT ()) 
                    -> LookAction
                    -> LookAction 
updateDisplay f lookAction@(LookAction {..}) =
  lookAction {_updatePerception' = updatePerception {_updateDisplay' = f}}
  where 
    updatePerception = _updatePerception'

updateBlockReport :: (Object -> GameStateExceptT ()) 
                      -> LookAction 
                      -> LookAction
updateBlockReport f lookAction@(LookAction {..}) =  
  lookAction {_updatePerception' = updatePerception {_updateBlockReport' = f}}
  where
    updatePerception = _updatePerception' 
    
updateLookPerception :: (LookF -> LookF) -> LookAction -> LookAction 
updateLookPerception f lookAction@(LookAction {..}) = 
  lookAction {_perception' = perception {_lookPerceptionF' = f}}
  where 
    perception = _perception'

updateDisplayPerception :: (Text -> Maybe Text) -> LookAction -> LookAction 
updateDisplayPerception f lookAction@(LookAction {..}) = 
  lookAction {_perception' = perception {_displayPerceptionF' = f}}
  where 
    perception = _perception'

canSeeF :: LookF -> LookF 
canSeeF lookF = lookF 

noSeeF :: LookF -> LookF 
noSeeF _ = const (const noSee) 

noSee :: GameStateExceptT ()
noSee = updateEnvironmentM msg 
  where
    msg = "You don't see that here."

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
