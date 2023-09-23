module Build.Locations.Kitchen.Exits.EastExit.Door.Actions.NoCanDo where
import Game.Model.World 
        (GetAction (..), GameStateExceptT, GoAction (..), LockAction (..)
        , PutAction (..), UnlockAction (..))
import Game.Model.Display (updateEnvironmentM)

getAction :: GetAction 
getAction = GetAction {
    _updateGet' = pass
  , _get'       = const get' 
}

get' :: GameStateExceptT ()
get' = updateEnvironmentM msg 
  where
    msg = "The door holds firm. There's no way to take it."

goAction :: GoAction
goAction = GoAction {
    _updateGo' = pass 
  , _go' = const (const go) 
}

go :: GameStateExceptT ()
go = updateEnvironmentM msg 
  where
    msg = "I know what you are trying to do, but there's another way."
    
lockAction :: LockAction 
lockAction = LockAction {
    _updateLock' = pass 
  , _lock' = lock 
}

lock :: GameStateExceptT () 
lock = updateEnvironmentM msg 
  where
    msg = "There's no lock on this door."

putAction :: PutAction
putAction = PutAction {
    _updatePut' = pass
  , _put' = const (const put')
}

put' :: GameStateExceptT ()
put' = updateEnvironmentM "You can't put anything on this door."

unlockAction :: UnlockAction 
unlockAction = UnlockAction {
    _updateUnlock' = pass 
  , _unlock' = unlock 
}

unlock :: GameStateExceptT () 
unlock = updateEnvironmentM msg 
  where
    msg = "There's no lock on this door."