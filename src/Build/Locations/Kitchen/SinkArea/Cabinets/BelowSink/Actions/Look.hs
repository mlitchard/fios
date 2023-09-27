module Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Actions.Look 
  where

import Game.Model.World
        (GameStateExceptT, Object (..), LookAction (..), LookAtF (..)
        , LookOnF (..), LookInF (..), LookF)
import Game.Actions.Look.StandardLook
        (makeLookAction, lookAtOpenBoxM, lookInClosedBoxM, lookInOpenBoxM, changeLookAction)
import Build.ObjectTemplate (kitchenCabinetBelowSinkGID)
import Game.Model.Display (updateEnvironmentM)
import Game.Object (setObjectMapM)

initialLookAction :: LookAction 
initialLookAction = closedCabinetLookAction

openCabinetLookAction :: LookAction 
openCabinetLookAction = 
  makeLookAction changeToCloseLook lookAtOpenF lookOnF lookInOpenF 

canDoLook :: LookF -> LookF 
canDoLook lookF = lookF 

cannotDoLook :: LookF -> LookF 
cannotDoLook _ = const (const (updateEnvironmentM noSeeMsg)) 
  where 
    noSeeMsg = "You can't see that cabinet."

canDisplayCabinet :: LookAtF -> LookAtF 
canDisplayCabinet lookAtF = lookAtF 

cannotDisplayCabinet :: LookAtF -> LookAtF 
cannotDisplayCabinet _ = LookAtF (const (const pass))

updateLookActionObject :: Object -> GameStateExceptT ()
updateLookActionObject = setObjectMapM kitchenCabinetBelowSinkGID

changeToCloseLook :: Object -> GameStateExceptT ()
changeToCloseLook entity = 
  updateLookActionObject $ changeLookAction closedCabinetLookAction entity
  
changeToOpenLook :: Object -> GameStateExceptT () 
changeToOpenLook entity = 
  updateLookActionObject $ changeLookAction openCabinetLookAction entity
  
closedCabinetLookAction :: LookAction 
closedCabinetLookAction = 
  makeLookAction changeToOpenLook lookAtClosedF lookOnF lookInClosedF 

cabinetOpen :: GameStateExceptT () 
cabinetOpen = updateEnvironmentM msg 
  where 
    msg = "The door is open"
    
cabinetClosed :: GameStateExceptT () 
cabinetClosed = updateEnvironmentM msg 
  where 
    msg = "The door is closed"

lookAtOpenF :: LookAtF
lookAtOpenF = LookAtF $ lookAtOpenBoxM kitchenCabinetBelowSinkGID

lookAtClosedF :: LookAtF
lookAtClosedF = LookAtF (const (const lookInClosedBoxM))

lookOnF :: LookOnF
lookOnF = LookOnF $ const (const (updateEnvironmentM msg)) 
  where
    msg = "The cabinet is flush with the shelf, there can't be anything on it."

lookInClosedF :: LookInF
lookInClosedF = LookInF (const (const lookInClosedBoxM))

lookInOpenF :: LookInF
lookInOpenF = LookInF $ lookInOpenBoxM kitchenCabinetBelowSinkGID
