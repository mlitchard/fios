module Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf.Actions.Open
  where 
import Game.Model.World (OpenAction (..))
import Control.Monad.Except (MonadError(throwError))

openAction :: OpenAction 
openAction = OpenAction {
    _updateOpen' = throwError "tbd"
  , _open' = const (throwError "tbd")
}