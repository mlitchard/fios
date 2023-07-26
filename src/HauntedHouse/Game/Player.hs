module HauntedHouse.Game.Player where 
import HauntedHouse.Game.Location.LocationData (Location)
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Object.Domain (Object)

data PlayerData = PlayerData 
  { _playerLocation :: GID Location
  , _p_inv :: [GID Object]
  } deriving stock Show 