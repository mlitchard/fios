module Build.Locations.Kitchen.SinkArea.Actions.Close where
import Game.Model.World (CloseAction (..))
import Control.Monad.Except (MonadError(throwError))

closeAction :: CloseAction
closeAction = CloseAction {
    _updateClose' = throwError "unfinished"
  , _close' = const (throwError "unfinished")
}