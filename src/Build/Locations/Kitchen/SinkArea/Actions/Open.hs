module Build.Locations.Kitchen.SinkArea.Actions.Open where
import Game.Model.World (OpenAction (..))
import Control.Monad.Except (MonadError(throwError))

openAction :: OpenAction
openAction = OpenAction {
    _updateOpen' = throwError "unfinished"
  , _open' = const (throwError "unfinished")
}