module Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf.Actions.Look 
  where
import Build.ObjectTemplate (kitchenCabinetAboveShelfGID)
import Game.Model.World
        (GameStateExceptT, Object, LookAction, LookAtF (..), LookOnF (..)
          , LookInF (..))
import Game.Object (setObjectMapM)
import Game.Actions.Look.StandardLook
        (makeLookAction, changeLookAction, lookAtOpenBoxM, lookInOpenBoxM
        , lookAtClosedBoxM, lookInClosedBoxM)
import Game.Model.Display (updateEnvironmentM)

initialLookAction :: LookAction 
initialLookAction = closedCabinetLookAction

updateLookActionObject :: Object -> GameStateExceptT ()
updateLookActionObject = setObjectMapM kitchenCabinetAboveShelfGID

openCabinetLookAction :: LookAction 
openCabinetLookAction = 
  makeLookAction changeToCloseLook lookAtOpenF lookOnF lookInOpenF

changeToCloseLook :: Object -> GameStateExceptT ()
changeToCloseLook entity = 
  updateLookActionObject $ changeLookAction closedCabinetLookAction entity

changeToOpenLook :: Object -> GameStateExceptT () 
changeToOpenLook entity = 
  updateLookActionObject $ changeLookAction openCabinetLookAction entity

closedCabinetLookAction :: LookAction 
closedCabinetLookAction = 
  makeLookAction changeToOpenLook lookAtClosedF lookOnF lookInClosedF

lookAtClosedF :: LookAtF
lookAtClosedF = LookAtF $ flip (const lookAtClosedBoxM) 

lookInClosedF :: LookInF 
lookInClosedF = LookInF $ const (const lookInClosedBoxM)

lookAtOpenF :: LookAtF 
lookAtOpenF = LookAtF $ lookAtOpenBoxM kitchenCabinetAboveShelfGID

lookInOpenF :: LookInF 
lookInOpenF = LookInF $ lookInOpenBoxM kitchenCabinetAboveShelfGID

lookOnF :: LookOnF 
lookOnF = LookOnF $ const (const (updateEnvironmentM msg)) 
  where
    msg = "There's about five cenitimeters between this cabinet and the ceiling"
            <> "So you can't really look on it. "
            <> "But, it's dust bunnies. Just dust bunnies." 