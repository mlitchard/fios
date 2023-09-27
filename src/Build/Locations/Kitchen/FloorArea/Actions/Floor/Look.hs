module Build.Locations.Kitchen.FloorArea.Actions.Floor.Look where
import Game.Model.World
        (LookAction, LookAtF (..), LookOnF (..)
        , LookInF (..), LookF, GameStateExceptT, GameState)
import Game.Actions.Look.StandardLook (makeLookAction, lookInOpenBoxM)
import Build.ObjectTemplate (kitchenFloorGID)
import Game.Model.Display (updateEnvironmentM)

lookAction :: LookAction
lookAction =
  makeLookAction (const pass) lookAtF lookOnF lookInF canPercieveF canDisplayF

-- canDisplayF :: LookF -> GameStateExceptT () 
canPercieveF :: GameStateExceptT () -> GameStateExceptT ()
canPercieveF lookAction = lookAction

-- canDisplayF :: GameStateExceptT () -> GameStateExceptT ()
canDisplayF _ = lookFloor "On the floor you see"

lookAtF :: LookAtF
lookAtF = LookAtF (lookFloor "You look at the floor.")

lookOnF :: LookOnF
lookOnF = LookOnF (lookFloor "you look on the floor.")

lookFloor :: Text -> LookF
lookFloor = lookInOpenBoxM kitchenFloorGID

lookInF :: LookInF
lookInF = LookInF $ const (const (updateEnvironmentM msg))
  where
    msg = "You can look on the floor, or at it. But not in it. It's a floor."