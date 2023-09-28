module Build.Locations.Kitchen.SinkArea.Actions.Look where
import Game.Model.World
        (GameStateExceptT, Object, LookAction (..), LookAtF (..), LookOnF (..)
        , LookInF (..), UpdatePerceptionFunctions (..), PerceptionFunctions (..), LookFunctions (..))
import Game.Object (setObjectMapM)
import Build.ObjectTemplate (kitchenSinkGID)
import Game.Actions.Look.StandardLook 
        (lookAtOpenBoxM, lookInOpenBoxM)
import Game.Model.Display (updateEnvironmentM)
import Game.Actions.Look.Update

updateLookActionObject :: Object -> GameStateExceptT ()
updateLookActionObject = setObjectMapM kitchenSinkGID 

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
    _lookAt' = lookAtF
  , _lookIn' = lookInF
  , _lookOn' = lookOnF
}

lookAction :: LookAction -> LookAction
lookAction =
  updateOpenReport (const pass)
    . updateLookAtF lookAtF
    . updateLookOnF lookOnF
    . updateLookInF lookInF

lookAtF :: LookAtF 
lookAtF = LookAtF $ lookAtOpenBoxM kitchenSinkGID

lookInF :: LookInF 
lookInF = LookInF $ lookInOpenBoxM kitchenSinkGID

lookOnF :: LookOnF 
lookOnF = LookOnF $ const (const lookOn)

lookOn :: GameStateExceptT ()
lookOn = updateEnvironmentM msg 
  where 
    msg = "You can look at a sink. Or you can look in a sink."