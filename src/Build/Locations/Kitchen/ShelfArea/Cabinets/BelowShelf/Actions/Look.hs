module Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf.Actions.Look 
  where

import Game.Model.World
import Build.ObjectTemplate (kitchenCabinetBelowShelfGID)
import Game.Object (setObjectMapM)
import Game.Actions.Look.StandardLook 
        (changeLookAction, lookAtOpenBoxM, lookInOpenBoxM
        , lookInClosedBoxM, lookAtClosedBoxM)
import Game.Model.Display (updateEnvironmentM)
import Game.Actions.Look.Update

initialLookAction :: LookAction
initialLookAction = closedCabinetLookAction defaultLookAction

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
    _lookAt' = lookAtClosedF
  , _lookIn' = lookInClosedF
  , _lookOn' = lookOnF
}

updateLookActionObject :: Object -> GameStateExceptT ()
updateLookActionObject = setObjectMapM kitchenCabinetBelowShelfGID

openCabinetLookAction :: LookAction -> LookAction
openCabinetLookAction =
  updateOpenReport changeToCloseLook
    . updateLookAtF lookAtOpenF
    . updateLookOnF lookOnF
    . updateLookInF lookInOpenF
    
closedCabinetLookAction :: LookAction -> LookAction
closedCabinetLookAction =
    updateOpenReport changeToOpenLook
    . updateLookAtF lookAtClosedF
    . updateLookOnF lookOnF
    . updateLookInF lookInClosedF

changeToCloseLook :: Object -> GameStateExceptT ()
changeToCloseLook entity@(Object {..}) =
  updateLookActionObject $ changeLookAction closedCabinetLookAction' entity
  where
    closedCabinetLookAction' =
      closedCabinetLookAction (_standardActions'._lookAction')

changeToOpenLook :: Object -> GameStateExceptT ()
changeToOpenLook entity@(Object {..}) =
  updateLookActionObject $ changeLookAction openCabinetLookAction' entity
  where
    openCabinetLookAction' =
      openCabinetLookAction (_standardActions'._lookAction')

lookAtClosedF :: LookAtF
lookAtClosedF = LookAtF $ flip (const lookAtClosedBoxM) 

lookInClosedF :: LookInF 
lookInClosedF = LookInF $ const (const lookInClosedBoxM)

lookAtOpenF :: LookAtF 
lookAtOpenF = LookAtF $ lookAtOpenBoxM kitchenCabinetBelowShelfGID

lookInOpenF :: LookInF 
lookInOpenF = LookInF $ lookInOpenBoxM kitchenCabinetBelowShelfGID

lookOnF :: LookOnF 
lookOnF = LookOnF $ const (const (updateEnvironmentM msg)) 
  where
    msg = "This cabinet is flush with the shelf. "
            <> "There's no way to have anything on it." 

cabinetOpen :: GameStateExceptT () 
cabinetOpen = updateEnvironmentM msg 
  where 
    msg = "The door is open"
    
cabinetClosed :: GameStateExceptT () 
cabinetClosed = updateEnvironmentM msg 
  where 
    msg = "The door is closed"

lookOn :: GameStateExceptT ()
lookOn = updateEnvironmentM msg 
  where
    msg = "The cabinet is flush with the shelf, there can't be anything on it."
