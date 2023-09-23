module Build.Locations.Kitchen.Exits.EastExit.Door.Actions.Close where
import Game.Model.World (CloseAction (..))

initialCloseAction :: CloseAction
initialCloseAction = CloseAction {
    _updateClose' = pass 
  , _close' = const pass 
}