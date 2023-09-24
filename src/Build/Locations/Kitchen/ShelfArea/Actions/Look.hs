module Build.Locations.Kitchen.ShelfArea.Actions.Look where

import Game.Actions.Look.StandardLook
import Game.Model.World
        (LookAction (..), LookInF (..), LookAtF (..), LookOnF (..), GameStateExceptT, Object, Container)
import Game.Model.Display (updateEnvironmentM)
import Game.Model.GID (GID)
import Build.ObjectTemplate (kitchenShelfGID)

lookAction :: LookAction 
lookAction = makeLookAction (const pass) lookAtF lookOnF lookInF

lookInF :: LookInF
lookInF = LookInF $ const (const (updateEnvironmentM msg)) 
  where 
    msg = "You can look on the shelf, or at it. But not in it. It's a shelf."

lookAtF :: LookAtF
lookAtF = LookAtF lookShelf   

lookOnF :: LookOnF 
lookOnF = LookOnF lookShelf  

lookShelf :: Object
              -> Map (GID Object) Container
              -> GameStateExceptT ()
lookShelf = lookInOpenBoxM kitchenShelfGID