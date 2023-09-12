{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use one" #-}
module HauntedHouse.Build.Locations.Kitchen.ShelfArea.Shelf
  where

import qualified Data.List.NonEmpty

import HauntedHouse.Game.Model.Mapping
        (GIDToDataMapping (..), ContainerMap (..), GIDList, Label (..))
import HauntedHouse.Game.Model.World
    ( World(_objectMap'), Object(..), ContainedOn (..), Containment (..)
    , GameStateExceptT, GameState (..), Nexus (..), RoomAnchor (EastAnchor), Orientation (Anchoring), StandardActions (..))
import qualified Data.Map.Strict (insert, singleton, empty)
import HauntedHouse.Build.ObjectTemplate
    ( kitchenShelfGID, plantPotGID )
import Data.These (These(..))
import HauntedHouse.Game.Model.Condition (Moveability(..), Perceptibility (..))
import HauntedHouse.Tokenizer (Lexeme(..))
import HauntedHouse.Game.Actions.Get 

buildKitchenShelf :: GameStateExceptT ()
buildKitchenShelf = do
  world <- _world' <$> get
  let objectMap' :: GIDToDataMapping Object
      objectMap' =
        GIDToDataMapping $ Data.Map.Strict.insert kitchenShelfGID buildShelf
          $ (_unGIDToDataMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}})

buildShelf :: Object
buildShelf= Object {
      _shortName' = "shelf"
    , _odescription' = [desc]
    , _descriptives' = []
    , _moveability' = NotMoveable
    , _perceptability' = Perceptible
    , _orientation' = orientation
    , _mNexus' =  (Just . Containment') shelfContainer
    , _standardActions' = standardActions 
  }
  where
    desc = "It's a shelf. You can put things on it"

standardActions :: StandardActions
standardActions = StandardActions 
  { _get' = noGetM
  , _put' = pass 
  }
  
orientation :: Orientation 
orientation = Anchoring EastAnchor

shelfContainer :: Containment
shelfContainer = (Containment . That) containedOn
  where
    containedOn :: ContainedOn
    containedOn = (ContainedOn . ContainerMap) Data.Map.Strict.empty

