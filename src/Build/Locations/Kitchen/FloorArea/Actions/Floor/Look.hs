module Build.Locations.Kitchen.FloorArea.Actions.Floor.Look where 
import Game.Model.World
        (LookAction, LookAtF (..), LookOnF (..), Object, Container
        , GameStateExceptT, LookInF (..))
import Game.Actions.Look.StandardLook (makeLookAction, lookInOpenBoxM)
import Build.ObjectTemplate (kitchenFloorGID)
import Game.Model.GID (GID)
import Game.Model.Display (updateEnvironmentM)

lookAction :: LookAction 
lookAction = makeLookAction (const pass) lookAtF lookOnF lookInF

lookAtF :: LookAtF
lookAtF = LookAtF lookFloor

lookOnF :: LookOnF 
lookOnF = LookOnF lookFloor  

lookFloor :: Object
              -> Map (GID Object) Container
              -> GameStateExceptT ()
lookFloor = lookInOpenBoxM kitchenFloorGID

lookInF :: LookInF
lookInF = LookInF $ const (const (updateEnvironmentM msg)) 
  where 
    msg = "You can look on the floor, or at it. But not in it. It's a floor."


