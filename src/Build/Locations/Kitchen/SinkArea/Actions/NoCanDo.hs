module Build.Locations.Kitchen.SinkArea.Actions.NoCanDo where 
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
    msg = "The sink is fastened to the wall and won't budge."

goAction :: GoAction 
goAction = GoAction {
    _updateGo' = pass 
  , _go' = const (const (throwError "Sink should never encouter a go action.")) 
}
lockAction :: LockAction 
lockAction = LockAction {
    _updateLock' = pass 
  , _lock' = lock
}

lock :: GameStateExceptT ()
lock = updateEnvironmentM msg 
  where
    msg = "Sinks generally can't be locked, and this sink is no exception."

unlockAction :: UnlockAction 
unlockAction = UnlockAction {
    _updateUnlock' = pass
  , _unlock' = unlock 
}

unlock :: GameStateExceptT ()
unlock = updateEnvironmentM msg 
  where
    msg = "Sinks generally can't be locked, and this sink is no exception."
