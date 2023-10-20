module Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Actions.Look
  where

import Game.Model.World
        (GameStateExceptT, Object (..), LookAction (..), LookAtF (..)
        , LookOnF (..), LookInF (..), LookF, UpdatePerceptionFunctions (..)
        , PerceptionFunctions (..), LookFunctions (..), StandardActions (..))

import Game.Actions.Look.StandardLook
        (lookAtOpenBoxM, lookInClosedBoxM, lookInOpenBoxM, changeLookAction, lookAtClosedBoxM, )
import Build.ObjectTemplate (kitchenCabinetBelowSinkGID)
import Game.Model.Display (updateEnvironmentM)
import Game.Object (setObjectMapM)
import Game.Actions.Look.Update (updateOpenReport, updateLookAtF, updateLookOnF, updateLookInF)

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
  , _displayPerceptionF' = Just
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

canDoLook :: LookF -> LookF
canDoLook lookF = lookF

cannotDoLook :: LookF -> LookF
cannotDoLook _ = const (const (updateEnvironmentM noSeeMsg))
  where
    noSeeMsg = "You can't see that cabinet."

cabinet :: LookAtF -> LookAtF
cabinet lookAtF = lookAtF

cannotDisplayCabinet :: LookAtF -> LookAtF
cannotDisplayCabinet _ = LookAtF (const (const pass))

updateLookActionObject :: Object -> GameStateExceptT ()
updateLookActionObject = setObjectMapM kitchenCabinetBelowSinkGID

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

cabinetOpen :: GameStateExceptT ()
cabinetOpen = updateEnvironmentM msg
  where
    msg = "The door is open"

cabinetClosed :: GameStateExceptT ()
cabinetClosed = updateEnvironmentM msg
  where
    msg = "The door is closed"

lookAtOpenF :: LookAtF
lookAtOpenF = LookAtF $ lookAtOpenBoxM kitchenCabinetBelowSinkGID

lookAtClosedF :: LookAtF
lookAtClosedF = LookAtF (flip (const lookAtClosedBoxM))

lookOnF :: LookOnF
lookOnF = LookOnF $ const (const (updateEnvironmentM msg))
  where
    msg = "The cabinet is flush with the shelf, there can't be anything on it."

lookInClosedF :: LookInF
lookInClosedF = LookInF (const (const lookInClosedBoxM))

lookInOpenF :: LookInF
lookInOpenF = LookInF $ lookInOpenBoxM kitchenCabinetBelowSinkGID
