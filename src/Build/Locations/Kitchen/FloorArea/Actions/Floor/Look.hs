module Build.Locations.Kitchen.FloorArea.Actions.Floor.Look where
import Game.Model.World
        (LookAction (..), LookAtF (..), LookOnF (..)
        , LookInF (..), GameStateExceptT, Object
        , UpdatePerceptionFunctions (..), PerceptionFunctions (..)
        , LookFunctions (..), Container)
import Game.Actions.Look.StandardLook (lookInOpenBoxM)
import Build.ObjectTemplate (kitchenFloorGID)
import Game.Model.Display (updateEnvironmentM)
import Game.Object (setObjectMapM)
import Game.Model.GID (GID)

updateLookActionObject :: Object -> GameStateExceptT ()
updateLookActionObject = setObjectMapM kitchenFloorGID

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
    _lookAt' = lookAtF
  , _lookIn' = lookInF
  , _lookOn' = lookOnF
}

lookAtF :: LookAtF
lookAtF = LookAtF lookFloor

lookOnF :: LookOnF
lookOnF = LookOnF lookFloor

lookFloor :: Object 
              -> Map (GID Object) Container
              -> GameStateExceptT ()
lookFloor = lookInOpenBoxM kitchenFloorGID

lookInF :: LookInF
lookInF = LookInF $ const (const (updateEnvironmentM msg))
  where
    msg = "You can look on the floor, or at it. But not in it. It's a floor."