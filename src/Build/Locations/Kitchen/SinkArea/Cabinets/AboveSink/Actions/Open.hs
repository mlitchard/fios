module Build.Locations.Kitchen.SinkArea.Cabinets.AboveSink.Actions.Open
  where
import Game.Model.World (OpenAction (..))
import Control.Monad.Except (MonadError(throwError))

openAction :: OpenAction
openAction = OpenAction {
    _updateOpen' = throwError "not finished"
  , _open' = const (throwError "not finished")
}