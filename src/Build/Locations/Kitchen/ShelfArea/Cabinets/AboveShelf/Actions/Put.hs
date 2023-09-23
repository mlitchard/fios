module Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf.Actions.Put 
  where 
import Game.Model.World (PutAction (..), GameStateExceptT)
import Control.Monad.Except (throwError)

putAction :: PutAction 
putAction = PutAction {
    _updatePut' = throwError "tbd"
  , _put' = const (const put')
}

put' :: GameStateExceptT ()
put' = throwError "tbd" 