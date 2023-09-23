module Build.Locations.Kitchen.SinkArea.Cabinets.AboveSink.Actions.Close 
  where 
import Game.Model.World (CloseAction (..))
import Control.Monad.Except (MonadError(throwError))

closeAction :: CloseAction 
closeAction = CloseAction {
    _updateClose' = throwError "not done"
  , _close' = const (throwError "not done")
}