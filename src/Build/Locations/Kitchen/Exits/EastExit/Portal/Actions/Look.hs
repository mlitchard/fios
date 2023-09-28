module Build.Locations.Kitchen.Exits.EastExit.Portal.Actions.Look where
import Game.Model.World
        (LookAction (..), UpdatePerceptionFunctions (..)
        , PerceptionFunctions (..), LookFunctions (..), LookAtF (..)
        , LookInF (..), LookOnF (..))

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

lookOnF :: LookOnF
lookOnF = LookOnF $ const (const pass)

lookAtClosedF :: LookAtF
lookAtClosedF = LookAtF (const (const pass))

lookInClosedF :: LookInF
lookInClosedF = LookInF (const (const pass))