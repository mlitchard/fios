module Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf.Cabinet
  where
  
import Game.Model.Mapping (Label (..))
import Game.Model.World
import qualified Data.Map.Strict (empty)
import Build.ObjectTemplate (kitchenCabinetBelowShelfGID, kitchenShelfGID)
import Game.Model.Condition 
        (Moveability(..), Proximity (PlacedUnder))
import Tokenizer (Lexeme(CABINET))
import Game.World (initContainerMapM)
import Build.Locations.Kitchen.ShelfArea.Actions.NoCanDo
import Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf.Actions.Put 
        (putAction)
import Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf.Actions.Look 
        (initialLookAction)
import Game.Object (setObjectMapM, getProximity)
import Build.LocationTemplate (kitchenGID)

buildKitchenCabinetBelowShelf :: GameStateExceptT ()
buildKitchenCabinetBelowShelf = do
  setObjectMapM kitchenCabinetBelowShelfGID buildCabinet
  initContainerMapM kitchenCabinetBelowShelfGID cabinetContainer

buildCabinet :: Object 
buildCabinet = Object { 
      _shortName' = "cabinet"
    , _entityLabel' = Label CABINET
    , _odescription' = [desc]
    , _descriptives' = []
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
orientation = AnchoredTo' anchoredProximity

anchoredProximity :: GameStateExceptT Proximity
anchoredProximity = 
  getProximity kitchenGID EastSection kitchenShelfGID kitchenCabinetBelowShelfGID

cabinetContainer :: Container
cabinetContainer = Container Data.Map.Strict.empty
