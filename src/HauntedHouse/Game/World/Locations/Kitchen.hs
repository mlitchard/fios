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
      kitchenSinkLabel,
      kitchenLabel )
import HauntedHouse.Game.World.Objects (popObjectGID)
import HauntedHouse.Game.Labels (ObjectLabel)
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Object.Domain (Object)
import HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.Shelf
import HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.Cabinets
import HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkCabinets
import HauntedHouse.Game.World.Locations (populateLocation)

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
        _lgPairs <- mapM pairUp kitchenObjects
        mapM_ (populateLocation kitchenLabel) _lgPairs 
        pass

pairUp :: ObjectLabel 
            -> ExceptT Text (StateT InitState IO) 
                            (ObjectLabel, GID Object)
pairUp label = do
  gid <- popObjectGID label
  pure (label,gid)
        
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


