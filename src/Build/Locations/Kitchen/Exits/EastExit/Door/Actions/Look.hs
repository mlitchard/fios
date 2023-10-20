module Build.Locations.Kitchen.Exits.EastExit.Door.Actions.Look where
import Game.Model.World
    ( GameStateExceptT,
      LookAction(..),
      LookAtF(LookAtF),
      LookFunctions(..),
      LookInF(LookInF),
      LookOnF(LookOnF),
      Object(..),
      PerceptionFunctions(..),
      StandardActions(_lookAction'),
      UpdatePerceptionFunctions(..) )
import Game.Actions.Look.StandardLook (changeLookAction, lookAtOpenDoor, lookAtClosedDoor)
import Build.ObjectTemplate (kitchenEastDoorGID)
import Game.Object (setObjectMapM)
import Game.Model.Display (updateEnvironmentM)
import Game.Actions.Look.Update
    ( updateLookAtF, updateLookInF, updateLookOnF, updateOpenReport )

updateLookActionObject :: Object -> GameStateExceptT ()
updateLookActionObject = setObjectMapM kitchenEastDoorGID

defaultLookAction :: LookAction
defaultLookAction = LookAction {
    _updatePerception' = defaultUpdatePerceptions
  , _perception' = defaultPerception
  , _lookFunctions' = defaultLookFunctions
}

defaultUpdatePerceptions :: UpdatePerceptionFunctions
defaultUpdatePerceptions = UpdatePerceptionFunctions {
    _updateBlockReport' = const pass
  , _updateDisplay' = const pass
}

defaultPerception :: PerceptionFunctions
defaultPerception = PerceptionFunctions {
    _lookPerceptionF' = id
  , _displayPerceptionF' = Just
}

defaultLookFunctions :: LookFunctions
defaultLookFunctions = LookFunctions {
    _lookAt' = lookAtClosedF
  , _lookIn' = lookInF
  , _lookOn' = lookOnF
}

changeToCloseLook :: Object -> GameStateExceptT ()
changeToCloseLook entity@(Object {..}) =
  updateLookActionObject $ changeLookAction closedDoorLookAction' entity
  where
    closedDoorLookAction' =
      closedDoorLookAction (_standardActions'._lookAction')

changeToOpenLook :: Object -> GameStateExceptT ()
changeToOpenLook entity@(Object {..}) =
  updateLookActionObject $ changeLookAction openDoorLookAction' entity
  where
    openDoorLookAction' =
      openDoorLookAction (_standardActions'._lookAction')

openDoorLookAction :: LookAction -> LookAction
openDoorLookAction =
  updateOpenReport changeToCloseLook
    . updateLookAtF lookAtOpenF
    . updateLookOnF lookOnF
    . updateLookInF lookInF

initialLookAction :: LookAction 
initialLookAction = closedDoorLookAction defaultLookAction

closedDoorLookAction :: LookAction -> LookAction
closedDoorLookAction =
    updateOpenReport changeToOpenLook
    . updateLookAtF lookAtClosedF
    . updateLookOnF lookOnF
    . updateLookInF lookInF

lookAtOpenF :: LookAtF
lookAtOpenF = LookAtF $ const (const lookAtOpenDoor)

lookAtClosedF :: LookAtF 
lookAtClosedF = LookAtF $ const (const lookAtClosedDoor)

lookOnF :: LookOnF
lookOnF = LookOnF $ const (const (updateEnvironmentM msg))
  where 
    msg = "There's nothing on the east door."

lookInF :: LookInF
lookInF = LookInF $ const (const (updateEnvironmentM msg)) 
  where 
    msg = "You can look at a door. "
            <> "You can look on the door. "
            <> "But you can't look in a door"
