module Build.Locations.Kitchen.FloorArea.Actions.PlantPot.NoCanDo where 
import Game.Model.World (UnlockAction (..), GameStateExceptT, GoAction (..), LockAction (..), OpenAction (..), CloseAction (..))
import Game.Model.Display (updateEnvironmentM)

goAction :: GoAction
goAction = GoAction
  { _updateGo' = pass
  , _go' = const (const pass)
  }

lockAction :: LockAction
lockAction = LockAction
  { _updateLock' = pass
  , _lock' = lock
  }

lock :: GameStateExceptT ()
lock = updateEnvironmentM msg
  where
    msg = "That would be a neat trick, but you can't lock a plant pot."


unlockAction :: UnlockAction
unlockAction = UnlockAction
  { _updateUnlock' = pass
  , _unlock' = unlock
  }

unlock :: GameStateExceptT ()
unlock = updateEnvironmentM msg
  where
    msg = "You don't need me to tell you that"
            <> " you can't unlock something that"
            <> " can't be locked in the first place."

openAction :: OpenAction
openAction = OpenAction
  { _updateOpen' = pass
  , _open' = const open
  }

open :: GameStateExceptT ()
open = do
  updateEnvironmentM msg
  where
    msg = "Very funny, but we both know you can't open a plant pot"

closeAction :: CloseAction
closeAction = CloseAction
  { _updateClose' = pass
  , _close' = const close
  }

close :: GameStateExceptT ()
close =
  updateEnvironmentM msg
  where
    msg = "Having fun with the parser, I see. You can't close a plant pot."