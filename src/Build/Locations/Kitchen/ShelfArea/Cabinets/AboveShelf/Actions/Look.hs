module Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf.Actions.Look 
  where
import Game.Model.World
import Build.ObjectTemplate (kitchenCabinetAboveShelfGID)
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

updateLookActionObject :: Object -> GameStateExceptT ()
updateLookActionObject = setObjectMapM kitchenCabinetAboveShelfGID


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

lookAtClosedF :: LookAtF
lookAtClosedF = LookAtF $ flip (const lookAtClosedBoxM) 

lookInClosedF :: LookInF 
lookInClosedF = LookInF $ const (const lookInClosedBoxM)

lookAtOpenF :: LookAtF 
lookAtOpenF = LookAtF $ lookAtOpenBoxM kitchenCabinetAboveShelfGID

lookInOpenF :: LookInF 
lookInOpenF = LookInF $ lookInOpenBoxM kitchenCabinetAboveShelfGID

lookOnF :: LookOnF 
lookOnF = LookOnF $ const (const (updateEnvironmentM msg)) 
  where
    msg = "There's about five cenitimeters between this cabinet and the ceiling"
            <> "So you can't really look on it. "
            <> "But, it's dust bunnies. Just dust bunnies." 