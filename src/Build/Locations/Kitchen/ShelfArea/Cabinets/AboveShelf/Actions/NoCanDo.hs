module Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf.Actions.NoCanDo
  where
import Game.Model.World (GetAction (..), GameStateExceptT, LockAction (..), UnlockAction (..), GoAction (..))
import Game.Model.Display (updateEnvironmentM)
import Control.Monad.Except (MonadError(throwError))

getAction :: GetAction 
getAction = GetAction {
    _updateGet' = pass
  , _get' = const get'
}

get' :: GameStateExceptT ()
get' = updateEnvironmentM msg 
  where 
    msg = "This shelf is firmly fasted to the wall, and won't budge"

goAction :: GoAction 
goAction = GoAction {
    _updateGo' = pass
  , _go' = const (const (throwError "This shelf should never be tyring to use goAction"))
}
lockAction :: LockAction 
lockAction = LockAction {
    _updateLock' = pass 
  , _lock' = lock
}

lock :: GameStateExceptT ()
lock = updateEnvironmentM msg 
  where 
    msg = "This cabinet doesn't have a lock"

unlockAction :: UnlockAction 
unlockAction = UnlockAction {
    _updateUnlock' = pass 
  , _unlock' = lock
}

unlock :: GameStateExceptT ()
unlock = updateEnvironmentM msg 
  where 
    msg = "This cabinet doesn't have a lock"