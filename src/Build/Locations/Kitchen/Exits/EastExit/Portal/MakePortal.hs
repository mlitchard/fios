module Build.Locations.Kitchen.Exits.EastExit.Portal.MakePortal where 
import Game.Model.World
        (GameStateExceptT, Object (..), Orientation (..), StandardActions (..), AnchoredTo)
import Game.World (setWorldExitMapM)
import Build.ObjectTemplate (kitchenEastPortalGID)
import Build.LocationTemplate (hallGID, kitchenGID)
import Game.Object (setObjectMapM, getAnchoredTo)
import Game.Model.Mapping (Label(..))
import Tokenizer.Data (Lexeme(PORTAL))
import Build.Locations.Kitchen.Exits.EastExit.Portal.Actions.Look 
        (defaultLookAction)
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
  , _orientation' = orientation
  , _standardActions' = actions  
}
  where
    orientation = AnchoredTo' anchoredTo 

anchoredTo :: GameStateExceptT AnchoredTo
anchoredTo = 
  getAnchoredTo kitchenGID kitchenEastPortalGID 

actions :: StandardActions
actions = StandardActions {
    _getAction' = getAction
  , _putAction' = putAction
  , _lookAction' = defaultLookAction
  , _openAction' = openAction
  , _closeAction' = closeAction 
  , _lockAction' = lockAction 
  , _unlockAction' = unlockAction 
  , _goAction' = goAction
}