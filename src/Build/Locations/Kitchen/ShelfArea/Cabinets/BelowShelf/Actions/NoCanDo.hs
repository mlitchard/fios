module Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf.Actions.NoCanDo
  where
import Game.Model.World (GetAction (..), GameStateExceptT)
import Game.Model.Display (updateEnvironmentM)

getAction :: GetAction 
getAction = GetAction {
    _updateGet' = pass
  , _get' = const noGet  
}

noGet :: GameStateExceptT ()
noGet = updateEnvironmentM msg 
  where
    msg = "This shelf is firmly in place, it's not going to budge."