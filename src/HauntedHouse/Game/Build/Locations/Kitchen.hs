module HauntedHouse.Game.Build.Locations.Kitchen where

import HauntedHouse.Game.Build.LocationTemplate
import HauntedHouse.Game.Model (GameStateExceptT, GameState (..))
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.World
import qualified Data.Map.Strict
import Control.Monad.Except (throwError)
{-
data GameState = GameState
  { _world'         :: World
  , _report'        :: [Text]
  , _player'        :: Player
  , _narration'     :: Narration
  , _newScene'      :: Bool
  , _clarification' :: Maybe (NonEmpty Text)
  }
data World = World 
  { _objectMap'         :: GIDToDataMapping Object
  , _objectLabelMap'    :: LabelToGIDMapping Object
  , _locationMap'       :: GIDToDataMapping Location  
  , _locationLabelMap'  :: LabelToGIDMapping Location
  }

-}
buildKitchen :: GameStateExceptT ()
buildKitchen = do
  world <- _world' <$> get
  _ <- throwMaybe errmsg 
        <$> Data.Map.Strict.lookup kitchenGID $ unLocationMap world
  
  pass
    where
      unLocationMap = _unGIDMapping' . _locationMap'
      errmsg = "kitchen should have been in this map but wasn't"

throwMaybe :: Text -> Maybe a -> GameStateExceptT a 
throwMaybe _ (Just a) = pure a 
throwMaybe errmsg Nothing  = throwError errmsg  


