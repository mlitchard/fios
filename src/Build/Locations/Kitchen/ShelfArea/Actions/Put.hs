module Build.Locations.Kitchen.ShelfArea.Actions.Put where
import Game.Model.World (PutAction (..), GameStateExceptT)
import Control.Monad.Except (MonadError(..))

putAction :: PutAction 
putAction = PutAction {
    _updatePut' = updatePut 
  , _put' = const (const put')
} 

updatePut :: GameStateExceptT ()
updatePut = pass 

put' :: GameStateExceptT () 
put' = throwError "ShelfActions put not finished."

