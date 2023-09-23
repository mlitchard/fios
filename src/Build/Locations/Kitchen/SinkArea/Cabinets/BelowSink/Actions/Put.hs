module Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Actions.Put 
  where 
import Game.Model.World (GameStateExceptT, PutAction (..))
import Control.Monad.Except (MonadError(..))

putAction :: PutAction 
putAction = PutAction {
    _updatePut' = throwError "tbd"
  , _put' = const (const put')
}

put' :: GameStateExceptT ()
put' = throwError "tbd" 