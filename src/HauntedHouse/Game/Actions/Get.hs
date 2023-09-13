module HauntedHouse.Game.Actions.Get where
import HauntedHouse.Game.Model.World
import Control.Monad.Except (MonadError(..))
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Object (getObjectM)
import qualified Data.Map.Strict
import qualified Data.List.NonEmpty
import HauntedHouse.Game.World (setLocationMapM, removeEntityLabelMapM)
import HauntedHouse.Game.Player (setPlayerInventoryM)
import Prelude hiding (fromLabel)

noGetM :: GameStateExceptT ()
noGetM = throwError "Are you serious?"


standardGetM :: GID Object -- entity gotten from
                  -> GID Object -- entity gotten
                  -> GameStateExceptT ()
standardGetM fromGID removedGid = do

  -- 
  {-
  let floorInv = _floorInventory' location
  if gid `elem` floorInv
    then do
          let updatedInv = filter (/= gid) floorInv
          --    updatedLocation = location{_floorInventory' = updatedInv}
          setLocationMapM lid updatedLocation
          setPlayerInventoryM gid
    else throwError notHereMSG
    -}
  pass
  where
    notHereMSG = "It should be on the floor but it isn't"

