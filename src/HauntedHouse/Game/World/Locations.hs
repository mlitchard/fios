module HauntedHouse.Game.World.Locations  where

import HauntedHouse.Game.Location
import HauntedHouse.Game.World (World (..))
import qualified Data.Map.Strict
import HauntedHouse.Game.World.InitState
    ( InitStateT, InitState(..) )
import Control.Monad.Except (MonadError(throwError))

getLocationData :: LocationLabel -> InitStateT LocationData
getLocationData locationLabel = do
  locationMap' <- _unLocationMap . _locationMap . _world <$> get
  let mLocationData = Data.Map.Strict.lookup locationLabel locationMap'
  -- Data.Map.Strict.lookup locationLabel unLocationMap 
  case mLocationData of
    Just locationData -> pure locationData
    Nothing           -> throwError ("could now find" <> show locationLabel)