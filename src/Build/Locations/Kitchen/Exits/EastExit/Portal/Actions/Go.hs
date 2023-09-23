module Build.Locations.Kitchen.Exits.EastExit.Portal.Actions.Go where
import Game.Model.World (GoAction (..), GameStateExceptT)
import Control.Monad.Except (MonadError(throwError))

goAction :: GoAction
goAction = GoAction {
    _updateGo' = updateGo 
  , _go' = const (const go)  
}

updateGo :: GameStateExceptT ()
updateGo = throwError "PortalActions: updateGo unfinished"

go :: GameStateExceptT ()
go = throwError "PortalActions: go unfinished"