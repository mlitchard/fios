module Build.Locations.Kitchen.Exits.EastExit.Portal.Actions.Look where
import Game.Model.World (LookAction (..), GameStateExceptT, LookAtF (..), LookInF (..), LookOnF (..))
import Control.Monad.Except (MonadError(throwError))

lookAction :: LookAction 
lookAction = LookAction {
    _updateLook' = const pass
  , _lookAt' = LookAtF (const (const look))
  , _lookIn' = LookInF (const (const pass))
  , _lookOn' = LookOnF (const (const pass)) 
}

look :: GameStateExceptT () 
look = throwError "look through not implemented"