module Build.Locations.Kitchen.Exits.EastExit.Door.Actions.Look where
import Game.Model.World
import Game.Actions.Look.StandardLook (makeLookAction, changeLookAction)
import Build.ObjectTemplate (kitchenEastDoorGID)
import Game.Object (setObjectMapM, getObjectM)
import Game.Model.Display (updatePlayerActionM, updateEnvironmentM)
import qualified Data.Text

openDoorLookAction :: LookAction 
openDoorLookAction = 
  makeLookAction changeToCloseLook lookAtOpenF lookOnF lookInF

changeToCloseLook :: Object -> GameStateExceptT ()
changeToCloseLook entity = 
  updateLookActionObject $ changeLookAction closedDoorLookAction entity

updateLookActionObject :: Object -> GameStateExceptT ()
updateLookActionObject = setObjectMapM kitchenEastDoorGID

changeToOpenLook :: Object -> GameStateExceptT () 
changeToOpenLook entity = 
  updateLookActionObject $ changeLookAction openDoorLookAction entity

initialLookAction :: LookAction 
initialLookAction = closedDoorLookAction

closedDoorLookAction :: LookAction 
closedDoorLookAction = 
  makeLookAction changeToOpenLook lookAtClosedF lookOnF lookInF

--lookAtOpenF :: LookAtF
lookAtOpenF :: LookAtF
lookAtOpenF = LookAtF $ const (const (lookAt openMsg))
  where 
    openMsg = updateEnvironmentM "It's open."

lookAtClosedF :: LookAtF 
lookAtClosedF = LookAtF $ const (const (lookAt closedMsg))
  where 
    closedMsg = updateEnvironmentM "It's closed"

lookOnF :: LookOnF
lookOnF = LookOnF $ const (const (updateEnvironmentM msg))
  where 
    msg = "There's nothing on the east door."

lookInF :: LookInF
lookInF = LookInF $ const (const (updateEnvironmentM msg)) 
  where 
    msg = "You can look at a door. "
            <> "You can look on the door. "
            <> "But you can't look in a door"
lookAt :: GameStateExceptT () -> GameStateExceptT ()
lookAt openStateM = do 
  (Object {..}) <- getObjectM kitchenEastDoorGID
  updatePlayerActionM ("You look at the " <> _shortName')
  openStateM
  updateEnvironmentM (Data.Text.concat _odescription') 