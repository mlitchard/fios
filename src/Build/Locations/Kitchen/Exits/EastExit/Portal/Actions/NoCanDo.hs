module Build.Locations.Kitchen.Exits.EastExit.Portal.Actions.NoCanDo where
import Game.Model.World
        (CloseAction (..), GameStateExceptT, GetAction (..), LockAction (..)
        , OpenAction (..), PutAction (..), UnlockAction (..))
import Game.Model.Display (updateEnvironmentM)

closeAction :: CloseAction 
closeAction = CloseAction {
    _updateClose' = pass 
  , _close' = const close 
}

close :: GameStateExceptT ()
close = updateEnvironmentM msg 
  where
    msg = "It's the door you can open and close, not the way through."

getAction :: GetAction
getAction = GetAction {
    _updateGet' = pass 
  , _get' = const get' 
}

get' :: GameStateExceptT ()
get' = updateEnvironmentM msg 
  where
    msg = "You can't get that"

lockAction :: LockAction
lockAction = LockAction {
    _updateLock' = pass 
  , _lock' = lock 
}

lock :: GameStateExceptT () 
lock = updateEnvironmentM msg 
  where 
    msg = "You can't lock a portal."

openAction :: OpenAction 
openAction = OpenAction {
    _updateOpen' = pass 
  , _open' = const open 
}

open :: GameStateExceptT ()
open = updateEnvironmentM msg 
  where
    msg = "It's the door you can open and close, not the way through."

putAction :: PutAction
putAction = PutAction {
    _updatePut' = pass 
  , _put' = const (const put') 
}

put' :: GameStateExceptT ()
put' = updateEnvironmentM msg 
  where
    msg = "You can't do that."

unlockAction :: UnlockAction
unlockAction = UnlockAction {
    _updateUnlock' = pass 
  , _unlock' = unlock 
}

unlock :: GameStateExceptT () 
unlock = updateEnvironmentM msg 
  where 
    msg = "You can't unlock a portal."
