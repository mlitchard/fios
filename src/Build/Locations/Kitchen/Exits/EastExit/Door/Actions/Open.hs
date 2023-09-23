module Build.Locations.Kitchen.Exits.EastExit.Door.Actions.Open where
import Game.Model.World (OpenAction (..))

initialOpenAction :: OpenAction
initialOpenAction = OpenAction {
    _updateOpen' = pass 
  , _open' = const pass
}