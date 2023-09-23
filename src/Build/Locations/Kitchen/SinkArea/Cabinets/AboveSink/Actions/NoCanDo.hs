module Build.Locations.Kitchen.SinkArea.Cabinets.AboveSink.Actions.NoCanDo
  where
import Game.Model.World
        (GoAction(..), GameStateExceptT, LockAction (..), GetAction (..), UnlockAction (..))
import Control.Monad.Except (MonadError(throwError))
import Game.Model.Display (updateEnvironmentM)

getAction :: GetAction
getAction = GetAction {
    _updateGet' = pass
  , _get' = const get'
}

get' :: GameStateExceptT ()
get' = updateEnvironmentM msg
  where 
    msg = "This cabinet is firmly in place and won't go anywhere."

goAction :: GoAction 
goAction = GoAction {
    _updateGo' = pass 
  , _go' = const (const go) 
}

go :: GameStateExceptT ()
go = throwError "AboveSink should never see this go action"

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