{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use one" #-}
module HauntedHouse.Game.Build.Locations.Kitchen where

import HauntedHouse.Game.Build.DirectionTemplate
import HauntedHouse.Game.Build.LocationTemplate
import HauntedHouse.Game.Model (GameStateExceptT, GameState (..))
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.World
import qualified Data.Map.Strict
import Control.Monad.Except (throwError)
import HauntedHouse.Game.Build.Default (kitchenShelf, kitchenSink)
import qualified Data.List.NonEmpty
import HauntedHouse.Game.Build.ObjectTemplate (kitchenShelfGID, kitchenSinkGID, kitchenCabinetAboveShelfGID, kitchenCabinetBelowShelfGID, kitchenCabinetAboveSinkGID, kitchenCabinetBelowSinkGID)
import HauntedHouse.Game.Model.GID (GID)


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
  world :: World <- _world' <$> get
  let locationMap' :: Data.Map.Strict.Map (GID Location) Location
      locationMap' = unLocationMap world
  kitchen <- throwMaybe errmsg
              $ Data.Map.Strict.lookup kitchenGID locationMap'
  let updatedKitchen = kitchen{_exits = Just kitchenExits
                              , _objects = Just kitchenObjects
                              , _description = kitchenDescription}
      updatedMap = GIDToDataMapping 
                    $ Data.Map.Strict.insert 
                        kitchenGID updatedKitchen locationMap'
  modify' (\gs -> gs {_world' = world {_locationMap' = updatedMap}})
    where
      kitchenDescription = 
        "It's a test kitchen. It has a sink and a shelf." 
          <> "You a cabinet above both the sink and the shelf."
          <> "There are also cabinets below both as well."

      kitchenObjects = Objects $ Data.List.NonEmpty.fromList 
        [kitchenShelfGID
        , kitchenSinkGID
        , kitchenCabinetAboveShelfGID
        , kitchenCabinetBelowShelfGID
        , kitchenCabinetAboveSinkGID
        , kitchenCabinetBelowSinkGID]
      kitchenExits = LabelToGIDMapping
                      $  Data.Map.Strict.singleton eastLabel hallGID
      unLocationMap = _unGIDMapping' . _locationMap'
      errmsg = "kitchen should have been in this map but wasn't"

{-

data Location = Location
  { _description  :: Text
  , _objects      :: Maybe Objects
  , _exits        :: Maybe (LabelToGIDMapping Exit Location) -- Maybe (Data.Map.Strict.Map (Label Exit) (GID Exit))
  } deriving stock Show

-}
throwMaybe :: Text -> Maybe a -> GameStateExceptT a
throwMaybe _ (Just a) = pure a
throwMaybe errmsg Nothing  = throwError errmsg


