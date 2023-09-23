module Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf.Actions.Look 
  where

import Game.Model.World
import Build.ObjectTemplate (kitchenCabinetBelowShelfGID)
import Game.Object (setObjectMapM)
import Game.Actions.Look.StandardLook (makeLookAction, changeLookAction, lookAtOpenBoxM, lookInOpenBoxM, lookInClosedBoxM, lookAtClosedBoxM)
import Game.Model.Display (updateEnvironmentM)

initialLookAction :: LookAction 
initialLookAction = closedCabinetLookAction

updateLookActionObject :: Object -> GameStateExceptT ()
updateLookActionObject = setObjectMapM kitchenCabinetBelowShelfGID

openCabinetLookAction :: LookAction 
openCabinetLookAction = 
  makeLookAction changeToCloseLook lookAtOpenF lookOnF lookInOpenF

closedCabinetLookAction :: LookAction 
closedCabinetLookAction = 
  makeLookAction changeToOpenLook lookAtClosedF lookOnF lookInClosedF

changeToCloseLook :: Object -> GameStateExceptT ()
changeToCloseLook entity = 
  updateLookActionObject $ changeLookAction closedCabinetLookAction entity

changeToOpenLook :: Object -> GameStateExceptT () 
changeToOpenLook entity = 
  updateLookActionObject $ changeLookAction openCabinetLookAction entity

lookAtClosedF :: LookAtF
lookAtClosedF = LookAtF $ flip (const lookAtClosedBoxM) 

lookInClosedF :: LookInF 
lookInClosedF = LookInF $ const (const lookInClosedBoxM)

lookAtOpenF :: LookAtF 
lookAtOpenF = LookAtF $ lookAtOpenBoxM kitchenCabinetBelowShelfGID

lookInOpenF :: LookInF 
lookInOpenF = LookInF $ lookInOpenBoxM kitchenCabinetBelowShelfGID

lookOnF :: LookOnF 
lookOnF = LookOnF $ const (const (updateEnvironmentM msg)) 
  where
    msg = "This cabinet is flush with the shelf. "
            <> "There's no way to have anything on it." 
{-
lookAction :: (GID Object -> GameStateExceptT ()) 
                -> Object -> (Map (GID Object) Container -> GameStateExceptT ())
                -> Object -> (Map (GID Object) Container -> GameStateExceptT ())
                -> LookAction 
lookAction changeFunction lookAt lookIn = LookAction {
    _updateLook' = changeFunction 
  , _look' = look lookAt lookOn lookIn 
}

openCabinetLookAction :: LookAction 
openCabinetLookAction = lookAction changeToCloseLook' lookAtOpen lookInOpen
  where
    changeToCloseLook' = changeToCloseLook kitchenCabinetBelowShelfGID


changeToCloseLook :: GID Object -> Object -> GameStateExceptT ()
changeToCloseLook gid entity = do 
  updatedEntity <- changeLookAction closedCabinetLookAction entity
  setObjectMapM gid updatedEntity

changeToOpenLook :: GID Object -> Object -> GameStateExceptT () 
changeToOpenLook gid entity = do 
  updatedEntity <- changeLookAction openCabinetLookAction entity
  setObjectMapM gid updatedEntity
  
closedCabinetLookAction :: LookAction 
closedCabinetLookAction = 
  lookAction changeToOpenLook lookAtClosed' lookInClosed' 
  where 
    lookAtClosed' = const (const lookAtClosed)
    lookInClosed' = const (const lookInClosed)

cabinetOpen :: GameStateExceptT () 
cabinetOpen = updateEnvironmentM msg 
  where 
    msg = "The door is open"
    
cabinetClosed :: GameStateExceptT () 
cabinetClosed = updateEnvironmentM msg 
  where 
    msg = "The door is closed"

lookAtOpen :: Object -> GameStateExceptT ()
lookAtOpen = lookAtOpenBoxM kitchenCabinetBelowShelfGID

lookAtClosed :: Object -> GameStateExceptT () 
lookAtClosed = lookAtClosed 
lookOn :: GameStateExceptT ()
lookOn = updateEnvironment msg 
  where
    msg = "The cabinet is flush with the shelf, there can't be anything on it."

lookInClosed :: GameStateExceptT ()
lookInClosed = lookInClosedBoxM

lookInOpen :: Object -> (Map (GID Object) Container -> GameStateExceptT ())
lookInOpen = lookInOpenBoxM kitchenCabinetBelowShelfGID
-}