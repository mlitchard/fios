module Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf.Actions.Close 
  where
import Game.Model.World (CloseAction (..))
import Control.Monad.Except (MonadError(throwError))

closeAction :: CloseAction 
closeAction = CloseAction {
    _updateClose' = throwError "tbd"
  , _close' = const (throwError "tbd")
}