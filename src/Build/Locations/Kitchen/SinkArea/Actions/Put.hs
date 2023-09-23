module Build.Locations.Kitchen.SinkArea.Actions.Put where 
import Game.Model.World (PutAction (..), GameStateExceptT)
import Control.Monad.Except (MonadError(..))

putAction :: PutAction 
putAction = PutAction {
    _updatePut' = pass 
  , _put' = const (const put') 
} 

put' :: GameStateExceptT ()
put' = throwError "sink put action tbd"