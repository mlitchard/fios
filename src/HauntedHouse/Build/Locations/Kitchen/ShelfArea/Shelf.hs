{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use one" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module HauntedHouse.Build.Locations.Kitchen.ShelfArea.Shelf
  where
    
import HauntedHouse.Game.Model.Mapping
        (GIDToDataMapping (..), ContainerMap (..), Label (..))
import HauntedHouse.Game.Model.World
    ( World(_objectMap'), Object(..), ContainedOn (..), Containment (..)
    , GameStateExceptT, GameState (..), Nexus (..), RoomAnchor (EastAnchor), Orientation (Anchoring), StandardActions (..))
import qualified Data.Map.Strict (insert, empty)
import HauntedHouse.Build.ObjectTemplate
    ( kitchenShelfGID )
import Data.These (These(..))
import HauntedHouse.Game.Model.Condition (Moveability(..), Perceptibility (..))
import HauntedHouse.Game.Actions.Get 
import HauntedHouse.Tokenizer (Lexeme (SHELF))
import Control.Monad.Except (MonadError(..))
import HauntedHouse.Game.Actions.Look (lookAt,lookOn)

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
    , _entityLabel' = Label SHELF
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
  { _get' = const pass -- noGetM
  , _put' = const pass
  , _lookIn' = const . const (throwError shelfLookInErr) 
  , _lookAt' = lookAt  -- ToDo
  , _lookOn' = lookOn 
  }
  where
    shelfLookInErr = "You summon unearlthy concentration, "
                      <> "but fail to look inside the shelf."
  
orientation :: Orientation 
orientation = Anchoring EastAnchor

shelfContainer :: Containment
shelfContainer = (Containment . That) containedOn
  where
    containedOn :: ContainedOn
    containedOn = (ContainedOn . ContainerMap) Data.Map.Strict.empty

