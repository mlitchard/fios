module Build.Locations.Kitchen.FloorArea.Actions.Floor.Get where 
import Game.Model.World (GetAction (..))
import Control.Monad.Except (MonadError(throwError))

-- get needs to implement "get from"

-- verification
-- does object exist in location
-- get algorithm
-- create action to put Object GID into player inventory,
      -- If accessible, do it
      -- not? throw error 
-- create action to remove gid from container
-- a >>= b
getAction :: GetAction
getAction = GetAction {
    _updateGet' = pass 
  , _get' = const (throwError "tbd")
} 