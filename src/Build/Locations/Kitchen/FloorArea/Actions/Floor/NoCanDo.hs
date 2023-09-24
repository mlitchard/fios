module Build.Locations.Kitchen.FloorArea.Actions.Floor.NoCanDo where 
import Game.Model.World 
        (CloseAction (..), GameStateExceptT, GoAction (..), UnlockAction (..)
        , LockAction (..), OpenAction (..))
import Game.Model.Display (updateEnvironmentM)

closeAction :: CloseAction 
closeAction = CloseAction 
  {   _updateClose' = pass
    , _close' = const close
  }

close :: GameStateExceptT () 
close = updateEnvironmentM msg 
  where
    msg = "How are you going to close a floor ? Arble with a smidge of garble?"
    
goAction :: GoAction 
goAction = GoAction 
  {   _updateGo' = pass
    , _go' = const (const go)
  }
  
go :: GameStateExceptT ()
go = updateEnvironmentM msg 
  where
    msg = "While it might be possible to do that with some floors, "
            <> "you can't do that with this one."

lockAction :: LockAction 
lockAction = LockAction 
  {   _updateLock' = pass
    , _lock' = lock
  }
lock :: GameStateExceptT ()
lock = updateEnvironmentM msg 
  where 
    msg = "You chant the magic words to do the impossible. "
            <> "And yet somehow fail to lock the floor."

openAction :: OpenAction 
openAction = OpenAction {
    _updateOpen' = pass 
  , _open' = const open 
} 

open :: GameStateExceptT ()
open = updateEnvironmentM msg 
  where 
    msg = "very funny. How do you open a floor?"

unlockAction :: UnlockAction
unlockAction = UnlockAction 
  { _updateUnlock' = pass 
  , _unlock' = unlock
  }

unlock :: GameStateExceptT ()
unlock = updateEnvironmentM msg 
  where 
    msg = "Think hard about what you just tried to do, unlock the floor."