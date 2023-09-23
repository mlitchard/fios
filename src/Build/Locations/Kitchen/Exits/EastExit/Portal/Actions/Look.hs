module Build.Locations.Kitchen.Exits.EastExit.Portal.Actions.Look where
import Game.Model.World (LookAction (..), GameStateExceptT)
import Control.Monad.Except (MonadError(throwError))

lookAction :: LookAction 
lookAction = LookAction {
    _updateLook' = const pass
  , _look' = const (const look) 
}

look :: GameStateExceptT () 
look = throwError "look through not implemented"