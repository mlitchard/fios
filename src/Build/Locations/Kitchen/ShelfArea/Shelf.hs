module Build.Locations.Kitchen.ShelfArea.Shelf where
    
import Game.Model.Mapping (Label (..))
import Game.Model.World
    ( Object(..), Container (..) , GameStateExceptT
    , StandardActions (..), Orientation (..), Anchored, RoomSection (EastSection))
import qualified Data.Map.Strict (empty)
import Build.ObjectTemplate
    ( kitchenShelfGID )
import Game.Model.Condition (Moveability(..))
import Tokenizer (Lexeme (SHELF))
import Build.Locations.Kitchen.ShelfArea.Actions.NoCanDo 
import Build.Locations.Kitchen.ShelfArea.Actions.Put
import Game.World (initContainerMapM)
import Build.Locations.Kitchen.ShelfArea.Actions.Look (initialLookAction)
import Game.Object (setObjectMapM, getAnchored)
import Build.LocationTemplate (kitchenGID)

buildKitchenShelf :: GameStateExceptT ()
buildKitchenShelf = do
  setObjectMapM kitchenShelfGID buildShelf
  initContainerMapM kitchenShelfGID shelfContainer

buildShelf :: Object
buildShelf= Object {
      _shortName' = "shelf"
    , _entityLabel' = Label SHELF
    , _odescription' = [desc]
    , _descriptives' = []
    , _orientation' = orientation
    , _standardActions' = standardActions 
  }
  where
    desc = "It's a shelf. You can put things on it"

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
orientation = Anchor shelfAnchor -- (kitchenGID, EastAnchor)

shelfAnchor :: GameStateExceptT (Maybe (NonEmpty Anchored))
shelfAnchor = getAnchored kitchenGID EastSection kitchenShelfGID notAnchorMsg
  where
    notAnchorMsg = show kitchenShelfGID <> " is not an anchor"

shelfContainer :: Container
shelfContainer = Container Data.Map.Strict.empty