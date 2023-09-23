module Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Actions.NoCanDo
  where

import Game.Model.World 
import Game.Model.Display (updateEnvironmentM)
import Control.Monad.Except ( MonadError(throwError) )

unlockAction :: UnlockAction
unlockAction = UnlockAction {
    _updateUnlock' = pass 
  , _unlock' = unlock 
}

unlock :: GameStateExceptT ()
unlock = updateEnvironmentM msg 
  where
    msg = "this cabinet doesn't have a lock"
    
lockAction :: LockAction
lockAction = LockAction {
    _updateLock' = pass 
  , _lock' = lock 
}

lock :: GameStateExceptT ()
lock = updateEnvironmentM msg 
  where 
    msg = "This cabinet doesn't have a lock."

getAction :: GetAction
getAction = GetAction {
    _updateGet' = pass
  , _get' = const get' 
}

goAction :: GoAction 
goAction = GoAction {
    _updateGo' = pass
  , _go' = const (const go) 
}

go :: GameStateExceptT ()
go = throwError msg 
  where
    msg = "BelowSink Cabinet should never get a go action"

get' :: GameStateExceptT ()
get' = updateEnvironmentM msg 
  where
    msg = "This cabinet is staying right where it is."

