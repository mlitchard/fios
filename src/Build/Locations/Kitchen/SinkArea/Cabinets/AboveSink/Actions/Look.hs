module Build.Locations.Kitchen.SinkArea.Cabinets.AboveSink.Actions.Look 
  where
import Game.Model.World 
import Build.ObjectTemplate (kitchenCabinetAboveSinkGID)
import Game.Actions.Look.StandardLook (makeLookAction, lookAtOpenBoxM, lookInOpenBoxM, lookInClosedBoxM, lookAtClosedBoxM)
import Game.Model.Display (updateEnvironmentM)
import Game.Object (setObjectMapM)


openCabinetLookAction :: LookAction 
openCabinetLookAction = 
  makeLookAction changeToCloseLook lookAtOpenF lookOnF lookInOpenF

updateLookActionObject :: Object -> GameStateExceptT ()
updateLookActionObject = setObjectMapM kitchenCabinetAboveSinkGID

changeToCloseLook :: Object -> GameStateExceptT ()
changeToCloseLook entity = 
  updateLookActionObject $ changeLookAction closedCabinetLookAction entity

changeToOpenLook :: Object -> GameStateExceptT () 
changeToOpenLook entity = 
  updateLookActionObject $ changeLookAction openCabinetLookAction entity
  
closedCabinetLookAction :: LookAction 
closedCabinetLookAction = 
  makeLookAction changeToOpenLook lookAtClosedF lookOnF lookInClosedF 

changeLookAction :: LookAction -> Object -> Object 
changeLookAction lookAction entity@(Object {..}) = 
  let standardActions' = _standardActions'
  in entity{_standardActions' = standardActions' {_lookAction' = lookAction}}

cabinetOpen :: GameStateExceptT () 
cabinetOpen = updateEnvironmentM msg 
  where 
    msg = "The door is open"
    
cabinetClosed :: GameStateExceptT () 
cabinetClosed = updateEnvironmentM msg 
  where 
    msg = "The door is closed"

-- lookAtClosedF :: LookAtF 
lookAtClosedF :: LookAtF
lookAtClosedF = LookAtF $ flip (const lookAtClosedBoxM)  

lookAtOpenF :: LookAtF
lookAtOpenF = LookAtF (lookAtOpenBoxM kitchenCabinetAboveSinkGID)

lookOnF :: LookOnF
lookOnF = LookOnF (const (const (updateEnvironmentM msg))) 
  where
    msg = "The cabinet is flush with the shelf, there can't be anything on it."

lookInClosedF :: LookInF
lookInClosedF = LookInF (const (const lookInClosedBoxM))

lookInOpenF :: LookInF 
lookInOpenF = LookInF (lookInOpenBoxM kitchenCabinetAboveSinkGID)
