module HauntedHouse.Game.Actions.Get where
import HauntedHouse.Game.Model.World
import Control.Monad.Except (MonadError(..))
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Object (getObjectM)
import qualified Data.Map.Strict
import qualified Data.List.NonEmpty
import HauntedHouse.Game.World (setLocationMapM)
import HauntedHouse.Game.Player (setPlayerInventoryM)

noGetM :: GameStateExceptT ()
noGetM = throwError "Are you serious?"

-- assumes object is on floor
standardGetM :: GID Object -> GameStateExceptT ()
standardGetM gid = do
  lid <- getLocationIdM
  location <- getLocationM lid
  floorInv <- throwMaybeM notHereMSG $ _floorInventory' location
  if gid `elem` floorInv 
    then do
          let updatedInv = nonEmpty (Data.List.NonEmpty.filter (/= gid) floorInv)
              updatedLocation = location{_floorInventory' = updatedInv}
          setLocationMapM lid updatedLocation
          setPlayerInventoryM gid
    else throwError notHereMSG
  where
    notHereMSG = "It should be on the floor but it isn't"