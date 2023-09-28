module Build.Locations.Kitchen.Exits.EastExit.Door.Actions.Look where
import Game.Model.World
import Game.Actions.Look.StandardLook (changeLookAction)
import Build.ObjectTemplate (kitchenEastDoorGID)
import Game.Object (setObjectMapM, getObjectM)
import Game.Model.Display (updatePlayerActionM, updateEnvironmentM)
import qualified Data.Text
import Game.Actions.Look.Update 

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
  , _displayPerceptionF' = id
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
lookAtOpenF = LookAtF $ const (const (lookAt openMsg))
  where 
    openMsg = updateEnvironmentM "It's open."

lookAtClosedF :: LookAtF 
lookAtClosedF = LookAtF $ const (const (lookAt closedMsg))
  where 
    closedMsg = updateEnvironmentM "It's closed"

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
lookAt :: GameStateExceptT () -> GameStateExceptT ()
lookAt openStateM = do 
  (Object {..}) <- getObjectM kitchenEastDoorGID
  updatePlayerActionM ("You look at the " <> _shortName')
  openStateM
  updateEnvironmentM (Data.Text.concat _odescription') 