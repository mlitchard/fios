module Build.Locations.Kitchen.Exits.EastExit.Portal.MakePortal where 
import Game.Model.World 
        (GameStateExceptT, Object (..), Orientation (..), StandardActions (..)
        , RoomAnchor (EastAnchor))
import Game.World (setWorldExitMapM)
import Build.ObjectTemplate (kitchenEastPortalGID)
import Build.LocationTemplate (hallGID)
import Game.Object (setObjectMapM)
import Game.Model.Mapping (Label(..))
import Tokenizer.Data (Lexeme(PORTAL))
import Game.Model.Condition (Moveability(..), Perceptibility (..))
import Build.Locations.Kitchen.Exits.EastExit.Portal.Actions.Look (lookAction)
import Build.Locations.Kitchen.Exits.EastExit.Portal.Actions.NoCanDo
import Build.Locations.Kitchen.Exits.EastExit.Portal.Actions.Go (goAction)

kitchenEastPortal :: GameStateExceptT ()
kitchenEastPortal = do
  setWorldExitMapM kitchenEastPortalGID hallGID
  setObjectMapM kitchenEastPortalGID kitchenEastPortalEntity 

kitchenEastPortalEntity :: Object 
kitchenEastPortalEntity = Object {
    _shortName' = "The way east"
  , _entityLabel' = Label PORTAL 
  , _odescription' = mempty 
  , _descriptives' = mempty 
  , _moveability' = NotMoveable 
  , _perceptability' = Imperceptible 
  , _orientation' = orientation
  , _standardActions' = actions  
}

orientation :: Orientation 
orientation = Anchored EastAnchor 

actions :: StandardActions
actions = StandardActions {
    _getAction' = getAction
  , _putAction' = putAction
  , _lookAction' = lookAction
  , _openAction' = openAction
  , _closeAction' = closeAction 
  , _lockAction' = lockAction 
  , _unlockAction' = unlockAction 
  , _goAction' = goAction
}