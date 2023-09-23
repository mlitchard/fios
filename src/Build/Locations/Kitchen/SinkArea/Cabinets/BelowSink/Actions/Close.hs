module Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Actions.Close
  where 
import Game.Model.World (CloseAction (..))
import Control.Monad.Except (MonadError(throwError))

closeAction :: CloseAction 
closeAction = CloseAction {
    _updateClose' = throwError "nope"
  , _close' = const (throwError "nope")
}