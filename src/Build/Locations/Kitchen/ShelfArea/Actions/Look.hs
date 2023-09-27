module Build.Locations.Kitchen.ShelfArea.Actions.Look where

import Game.Actions.Look.StandardLook
import Game.Model.World
import Game.Model.Display (updateEnvironmentM)
import Game.Model.GID (GID)
import Build.ObjectTemplate (kitchenShelfGID)
import Game.Object (setObjectMapM)

updateLookActionObject :: Object -> GameStateExceptT ()
updateLookActionObject = setObjectMapM kitchenShelfGID 

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

lookInF :: LookInF
lookInF = LookInF $ const (const (updateEnvironmentM msg)) 
  where 
    msg = "You can look on the shelf, or at it. But not in it. It's a shelf."

lookAtF :: LookAtF
lookAtF = LookAtF lookShelf   

lookOnF :: LookOnF 
lookOnF = LookOnF lookShelf  

lookShelf :: Object
              -> Map (GID Object) Container
              -> GameStateExceptT ()
lookShelf = lookInOpenBoxM kitchenShelfGID