module HauntedHouse.Game.World.Locations.Kitchen where

import HauntedHouse.Game.World.InitState
import HauntedHouse.Game.World.Locations.Kitchen.KitchenSink
import HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.InitState
    ( makeShelf )
import HauntedHouse.Game.World.Labels
    ( kitchenCabinetAboveShelfLabel,
      kitchenCabinetBelowShelfLabel,
      kitchenShelfLabel,
      kitchenSinkCabinetAboveLabel,
      kitchenSinkCabinetBelowLabel,
      kitchenSinkLabel )
import HauntedHouse.Game.World.Objects (popObjectGID)
import HauntedHouse.Game.Labels (ObjectLabel)

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

  { _description    :: Text
  , _objectLabelMap  :: ObjectLabelMap
  , _exits          :: ExitMap 
  }
-}
makeKitchen :: InitStateT ()
makeKitchen = do
  makeKitchen'
  makeSink
  makeShelf
    where
      makeKitchen' :: InitStateT ()
      makeKitchen' = do
        lgPairs <- mapM popObjectGID kitchenObjects
        pass
      pairUp label = do
        gid <- popObjectGID label
        pure (label)
        
kitchenObjects :: [ObjectLabel]
kitchenObjects = [kitchenShelfLabel
                  , kitchenCabinetAboveShelfLabel
                  , kitchenCabinetBelowShelfLabel
                  , kitchenSinkLabel
                  , kitchenSinkCabinetAboveLabel
                  , kitchenSinkCabinetBelowLabel]
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


