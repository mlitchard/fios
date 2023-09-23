module Build.Locations.Kitchen.ShelfArea.Actions.NoCanDo where
import Game.Model.World 
        (CloseAction (..), GameStateExceptT, GetAction (..), LockAction (..)
        , OpenAction (..), UnlockAction (..), GoAction (..))
import Game.Model.Display (updateEnvironmentM)
import Control.Monad.Except (MonadError(throwError))

closeAction :: CloseAction 
closeAction = CloseAction {
    _updateClose' = pass 
  , _close' = const close
}

close :: GameStateExceptT ()
close = updateEnvironmentM msg 
  where 
    msg = "You can put things on a shelf. "
            <> "You can take things off of it. "
            <> "You can look at it. But you can't close it. It's a shelf."
          
getAction :: GetAction
getAction = GetAction {
    _updateGet' = pass 
  , _get' = const noget
}

noget :: GameStateExceptT ()
noget = updateEnvironmentM msg 
  where
    msg = "The shelf is firmly in place, and will remain that way."

lockAction :: LockAction 
lockAction = LockAction {
    _updateLock' = pass 
  , _lock' = lock
}

lock :: GameStateExceptT ()
lock = updateEnvironmentM msg 
  where 
    msg = "You can put things on a shelf. "
            <> "You can take things off of it. "
            <> "You can look at it. But you can't lock it. It's a shelf."

openAction :: OpenAction 
openAction = OpenAction {
    _updateOpen' = pass 
  , _open' = const open
}

open :: GameStateExceptT ()
open = updateEnvironmentM msg 
  where 
    msg = "You can put things on a shelf. "
            <> "You can take things off of it. "
            <> "You can look at it. But you can't open it. It's a shelf."

unlockAction :: UnlockAction 
unlockAction = UnlockAction {
    _updateUnlock' = pass 
  , _unlock' = open
}

unlock :: GameStateExceptT ()
unlock = updateEnvironmentM msg 
  where 
    msg = "You can put things on a shelf. "
            <> "You can take things off of it. "
            <> "You can look at it. But you can't unlock it. It's a shelf."

goAction :: GoAction 
goAction = GoAction {
    _updateGo' = pass 
  , _go' = const (const go) 
}

go :: GameStateExceptT ()
go = throwError "Arble and garble, I say to you."