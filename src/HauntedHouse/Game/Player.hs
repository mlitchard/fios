module HauntedHouse.Game.Player where 
import HauntedHouse.Game.Location.LocationData (LocationData)
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Object.Domain (Object)

data PlayerData = PlayerData 
  { _playerLocation :: GID LocationData
  , _p_inv :: [GID Object]
  } deriving stock Show 