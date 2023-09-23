module Build.Locations.Kitchen.ShelfArea.Shelf where
    
import Game.Model.Mapping
        (GIDToDataMapping (..), ContainerMap (..), Label (..))
import Game.Model.World
    ( World(_objectMap'), Object(..), Container (..)
    , GameStateExceptT, GameState (..), RoomAnchor (EastAnchor)
    , StandardActions (..), Orientation (..))
import qualified Data.Map.Strict (insert, empty)
import Build.ObjectTemplate
    ( kitchenShelfGID )
import Game.Model.Condition (Moveability(..), Perceptibility (..))
import Tokenizer (Lexeme (SHELF))
import Build.Locations.Kitchen.ShelfArea.Actions.NoCanDo 
import Build.Locations.Kitchen.ShelfArea.Actions.Put
import Game.World (initContainerMapM)
import Build.Locations.Kitchen.ShelfArea.Actions.Look (lookAction)

buildKitchenShelf :: GameStateExceptT ()
buildKitchenShelf = do
  world <- _world' <$> get
  let objectMap' :: GIDToDataMapping Object Object
      objectMap' =
        GIDToDataMapping $ Data.Map.Strict.insert kitchenShelfGID buildShelf
          $ (_unGIDToDataMapping' . _objectMap') world
  initContainerMapM kitchenShelfGID shelfContainer
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
    , _standardActions' = standardActions 
  }
  where
    desc = "It's a shelf. You can put things on it"

standardActions :: StandardActions
standardActions = StandardActions { 
    _getAction' = getAction
  , _putAction' = putAction 
  , _lookAction' = lookAction
  , _openAction' = openAction
  , _closeAction' = closeAction 
  , _lockAction' = lockAction
  , _unlockAction' = unlockAction
  , _goAction' = goAction
}
  
orientation :: Orientation 
orientation = Anchored EastAnchor

shelfContainer :: Container
shelfContainer = Container 
  $ ContainerMap Data.Map.Strict.empty