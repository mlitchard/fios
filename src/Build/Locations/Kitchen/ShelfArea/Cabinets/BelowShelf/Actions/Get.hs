module Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf.Actions.Get
  where
import Game.Model.World (GetAction (..))
import Control.Monad.Except (MonadError(throwError))

getAction :: GetAction
getAction = GetAction {
    _updateGet' = throwError "not finished"
  , _get' = const pass
}