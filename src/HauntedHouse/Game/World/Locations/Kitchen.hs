module HauntedHouse.Game.World.Locations.Kitchen where

import HauntedHouse.Game.World.InitState 
import HauntedHouse.Game.World.Locations.Kitchen.KitchenSink
import HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.InitState
    ( makeShelf )

{-
data World = World 
  { _objectMap      :: ObjectMap
  , _objectLabelMap :: ObjectLabelMap
  , _locationMap    :: LocationMap
  , _agentMap       :: AgentMap
  } deriving stock Show 

  newtype LocationMap = LocationMap
  { _unLocationMap :: Data.Map.Strict.Map LocationLabel LocationData}
     deriving stock (Show)
-}
makeKitchen :: InitStateT ()
makeKitchen = do
  makeKitchen'
  makeSink 
  makeShelf
    where
      makeKitchen' :: InitStateT () 
      makeKitchen' = pass -- do
{-
        (World objectMap objectLabelMap locationMap) <- _world <$> get
        let locationMap' = _unLocationMap locationMap 
            kitchen      = lookup kitchenLabel locationMap'
        case kitchen of 
              Nothing           -> throwError kitchenErr
              Just kitchenData' -> pass 
        where                          
          kitchenErr = "kitchen data should have been present but wasn't"
  -}


