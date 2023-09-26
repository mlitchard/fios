module Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf.Cabinet where

import Game.Model.World
import Game.Model.Mapping
import qualified Data.Map.Strict
import Build.ObjectTemplate 
        (kitchenCabinetAboveShelfGID, kitchenShelfGID)
import Game.Model.Condition 
        (Moveability(..), Perceptibility (..), Proximity (PlacedAbove))
import Tokenizer (Lexeme(SINK))
import Game.World (initContainerMapM)
import Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf.Actions.NoCanDo
import Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf.Actions.Put 
        (putAction)
import Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf.Actions.Look 
        (initialLookAction)
import Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf.Actions.Open 
        (openAction)
import Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf.Actions.Close 
        (closeAction)
import Game.Object (setObjectMapM)


{-
  setObjectMapM kitchenFloorGID buildFloor 
  initContainerMapM kitchenFloorGID floorContainer
-}
buildKitchenCabinetAboveShelf :: GameStateExceptT ()
buildKitchenCabinetAboveShelf = do
  setObjectMapM kitchenCabinetAboveShelfGID buildCabinet     
  initContainerMapM kitchenCabinetAboveShelfGID cabinetContainer

buildCabinet :: Object 
buildCabinet = Object {
      _shortName' = "cabinet"
    , _entityLabel' = Label SINK
    , _odescription' = [desc]
    , _descriptives' = []
    , _moveability' = NotMoveable
    , _perceptability' = Perceptible
    , _orientation' = orientation
    , _standardActions' = standardActions
  }
  where 
    desc = "You can put things in it."

standardActions :: StandardActions
standardActions = StandardActions { 
    _getAction' = getAction
  , _putAction' = putAction 
  , _lookAction' = initialLookAction
  , _openAction' = openAction
  , _closeAction' = closeAction 
  , _lockAction' = lockAction
  , _unlockAction' = unlockAction
  , _goAction' = goAction
}

orientation :: Orientation 
orientation = AnchoredTo' (kitchenShelfGID, PlacedAbove)

cabinetContainer :: Container
cabinetContainer = Container
  $ ContainerMap Data.Map.Strict.empty