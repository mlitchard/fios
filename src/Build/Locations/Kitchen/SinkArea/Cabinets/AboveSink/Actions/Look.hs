module Build.Locations.Kitchen.SinkArea.Cabinets.AboveSink.Actions.Look 
  where
import Game.Model.World 
import Build.ObjectTemplate (kitchenCabinetAboveSinkGID)
import Game.Actions.Look.StandardLook 
        (lookAtOpenBoxM, lookInOpenBoxM, lookInClosedBoxM, lookAtClosedBoxM)
import Game.Model.Display (updateEnvironmentM)
import Game.Object (setObjectMapM)
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
  , _lookIn' = lookInClosedF
  , _lookOn' = lookOnF
}

openCabinetLookAction :: LookAction -> LookAction
openCabinetLookAction =
  updateOpenReport changeToCloseLook
    . updateLookAtF lookAtOpenF
    . updateLookOnF lookOnF
    . updateLookInF lookInOpenF

updateLookActionObject :: Object -> GameStateExceptT ()
updateLookActionObject = setObjectMapM kitchenCabinetAboveSinkGID

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
  
closedCabinetLookAction :: LookAction -> LookAction
closedCabinetLookAction =
    updateOpenReport changeToOpenLook
    . updateLookAtF lookAtClosedF
    . updateLookOnF lookOnF
    . updateLookInF lookInClosedF

changeLookAction :: LookAction -> Object -> Object 
changeLookAction lookAction entity@(Object {..}) = 
  let standardActions' = _standardActions'
  in entity{_standardActions' = standardActions' {_lookAction' = lookAction}}

cabinetOpen :: GameStateExceptT () 
cabinetOpen = updateEnvironmentM msg 
  where 
    msg = "The door is open"
    
cabinetClosed :: GameStateExceptT () 
cabinetClosed = updateEnvironmentM msg 
  where 
    msg = "The door is closed"

-- lookAtClosedF :: LookAtF 
lookAtClosedF :: LookAtF
lookAtClosedF = LookAtF $ flip (const lookAtClosedBoxM)  

lookAtOpenF :: LookAtF
lookAtOpenF = LookAtF (lookAtOpenBoxM kitchenCabinetAboveSinkGID)

lookOnF :: LookOnF
lookOnF = LookOnF (const (const (updateEnvironmentM msg))) 
  where
    msg = "The cabinet is flush with the shelf, there can't be anything on it."

lookInClosedF :: LookInF
lookInClosedF = LookInF (const (const lookInClosedBoxM))

lookInOpenF :: LookInF 
lookInOpenF = LookInF (lookInOpenBoxM kitchenCabinetAboveSinkGID)
