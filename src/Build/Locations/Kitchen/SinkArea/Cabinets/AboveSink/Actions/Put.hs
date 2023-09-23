module Build.Locations.Kitchen.SinkArea.Cabinets.AboveSink.Actions.Put 
  where 
import Game.Model.World (PutAction (..), GameStateExceptT)
import Control.Monad.Except (MonadError(..))

putAction :: PutAction 
putAction = PutAction {
    _updatePut' = throwError "tbd"
  , _put' = const (const put')
}

put' :: GameStateExceptT ()
put' = throwError "tbd" 