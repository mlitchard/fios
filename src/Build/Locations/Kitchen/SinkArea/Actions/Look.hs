module Build.Locations.Kitchen.SinkArea.Actions.Look where
import Game.Model.World 
        (GameStateExceptT, Object, LookAction, LookAtF (..), LookOnF (..)
        , LookInF (..))
import Game.Object (setObjectMapM)
import Build.ObjectTemplate (kitchenSinkGID)
import Game.Actions.Look.StandardLook (makeLookAction, lookAtOpenBoxM, lookInOpenBoxM)
import Game.Model.Display (updateEnvironmentM)

updateLookActionObject :: Object -> GameStateExceptT ()
updateLookActionObject = setObjectMapM kitchenSinkGID 

lookAction :: LookAction 
lookAction = 
  makeLookAction (const pass) lookAtF lookOnF lookInF

lookAtF :: LookAtF 
lookAtF = LookAtF $ lookAtOpenBoxM kitchenSinkGID

lookInF :: LookInF 
lookInF = LookInF $ lookInOpenBoxM kitchenSinkGID

lookOnF :: LookOnF 
lookOnF = LookOnF $ const (const lookOn)

lookOn :: GameStateExceptT ()
lookOn = updateEnvironmentM msg 
  where 
    msg = "You can look at a sink. Or you can look in a sink."