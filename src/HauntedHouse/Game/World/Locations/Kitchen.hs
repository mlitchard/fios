module HauntedHouse.Game.World.Locations.Kitchen where

import HauntedHouse.Game.World.InitState
import HauntedHouse.Game.World.Locations.Kitchen.KitchenSink
import HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.InitState
    ( makeShelf )
import HauntedHouse.Game.World.Labels


-- import HauntedHouse.Game.World.Objects (popObjectGID, initObj)
import HauntedHouse.Game.Labels (ObjectLabel (..), LocationLabel (..))
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Object.Domain (Object)
import HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.Shelf
import HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.Cabinets
import HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkCabinets
import HauntedHouse.Game.World.Locations
import qualified Data.List.NonEmpty
import qualified Data.Map.Strict
import HauntedHouse.Tokenizer (Lexeme(..))
import HauntedHouse.Game.World.Locations 
import HauntedHouse.Game.Location (Location)

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
{-
initKitchen :: InitStateT ()
initKitchen = do
  sinkGID <- addSinkGID <$> popObjectGID (ObjectLabel SINK)
  shelfGID <- popObjectGID (ObjectLabel SHELF)
  cabinetGIDS <- mapM popObjectGID cabinetLabels
  mapM addObjectGID sinkGID : shelfGID : cabinetGIDS 
    where
      cabinetLabels :: [ObjectLabel]
      cabinetLabels = replicate 4 (ObjectLabel CABINET)

addObjectGID :: GID Object -> InitStateT ()
addObjectGID gid = do
  world <- _world' <$> get
  kitchen <- getLocation kitchenLabel head
  updateWorld <- addObject gid kitchenLabel kitchen
  updateWorld world

makeKitchenGIDs :: [(ObjectLabel,[GID Object])] -> InitStateT ()
makeKitchenGIDs _xs = pass

    where
      makeKitchen' :: InitStateT ()
      makeKitchen' = do
        lgPairs <- mapM pairUp kitchenObjects
        mapM_ (populateLocation kitchenLabel) lgPairs
        let used :: [(ObjectLabel, [GID Object])]
            used = map (initObj . Data.List.NonEmpty.fromList)
                    $ (group . sort ) lgPairs
        mapM_ updatePlaced used

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
-}

