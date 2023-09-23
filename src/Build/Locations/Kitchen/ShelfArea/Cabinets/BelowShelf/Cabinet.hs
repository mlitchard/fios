module Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf.Cabinet
  where
  
import Game.Model.Mapping 
        (GIDToDataMapping (..), ContainerMap (..), Label (..))
import Game.Model.World
import qualified Data.Map.Strict (insert, empty)
import Build.ObjectTemplate (kitchenCabinetBelowShelfGID, kitchenShelfGID)
import Game.Model.Condition 
        (Moveability(..), Perceptibility (..), Proximity (PlacedUnder))
import Tokenizer (Lexeme(CABINET))
import Game.World (initContainerMapM)
import Build.Locations.Kitchen.ShelfArea.Actions.NoCanDo
import Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf.Actions.Put (putAction)
import Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf.Actions.Look (initialLookAction)

buildKitchenCabinetBelowShelf :: GameStateExceptT ()
buildKitchenCabinetBelowShelf = do
  world <- _world' <$> get 
  let objectMap' :: GIDToDataMapping Object Object
      objectMap' = 
        GIDToDataMapping 
          $ Data.Map.Strict.insert 
              kitchenCabinetBelowShelfGID buildCabinet 
                $ (_unGIDToDataMapping' . _objectMap') world
  initContainerMapM kitchenCabinetBelowShelfGID cabinetContainer
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}})

buildCabinet :: Object 
buildCabinet = Object { 
      _shortName' = "cabinet"
    , _entityLabel' = Label CABINET
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
orientation = AnchoredTo' (kitchenShelfGID,PlacedUnder)

cabinetContainer :: Container
cabinetContainer = Container 
  $ ContainerMap Data.Map.Strict.empty


  