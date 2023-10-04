module Build.Locations.Kitchen.Exits.EastExit.Door.MakeDoor where 
import Game.Model.World
        (GameStateExceptT, Object (..), StandardActions (..), Orientation (..)
        , RoomSection (EastSection), Anchored)
import Build.ObjectTemplate (kitchenEastPortalGID, kitchenEastDoorGID)
import Game.Model.Condition (Proximity(PlacedFront), Moveability (..), Perceptibility (..))
import Build.Locations.Kitchen.Exits.EastExit.Portal.Actions.NoCanDo
import Build.Locations.Kitchen.Exits.EastExit.Door.Actions.Look 
        (initialLookAction)
import Game.Object (setObjectMapM, setObjectLabelMapM, getAnchored)
import Build.ObjectLabels (doorLabel)
import Build.LocationTemplate (kitchenGID)
import Game.World (setLocationDirectionM)
import Build.DirectionTemplate (eastLabel)
import Tokenizer (Lexeme(DOOR))
import Game.Model.Mapping (Label(..))
import Build.Locations.Kitchen.Exits.EastExit.Door.Actions.Open (initialOpenAction)
import Build.Locations.Kitchen.Exits.EastExit.Door.Actions.Close (initialCloseAction)
import Build.Locations.Kitchen.Exits.EastExit.Door.Actions.NoCanDo (goAction)

kitchenEastDoor :: GameStateExceptT ()
kitchenEastDoor = do
  setObjectMapM kitchenEastDoorGID kitchenEastDoorObject
  setObjectLabelMapM kitchenGID doorLabel kitchenEastDoorGID
  setLocationDirectionM  kitchenGID eastLabel kitchenEastDoorGID

kitchenEastDoorObject :: Object
kitchenEastDoorObject = Object {
      _shortName'      = kitchenShortName
    , _entityLabel' = Label DOOR
    , _odescription'   = [kitchenEastDoorDesc]
    , _descriptives'   = []
    , _orientation'    = orientation
    , _standardActions' = doorActions
  }
  where
    orientation = Anchor getDoorAnchored
    kitchenShortName    = "door to the east hall."
    kitchenEastDoorDesc = "It's a door made from some mysterious substance."

doorActions :: StandardActions
doorActions = StandardActions {
      _getAction' = getAction
    , _putAction' = putAction
    , _lookAction' = initialLookAction
    , _openAction' = initialOpenAction
    , _closeAction' = initialCloseAction
    , _lockAction' = lockAction
    , _unlockAction' = unlockAction
    , _goAction' = goAction
  }

getDoorAnchored :: GameStateExceptT (Maybe (NonEmpty Anchored))
getDoorAnchored = 
  getAnchored kitchenGID EastSection kitchenEastDoorGID notSection
  where 
    notSection = show kitchenEastDoorGID <> "is not in " <> show EastSection