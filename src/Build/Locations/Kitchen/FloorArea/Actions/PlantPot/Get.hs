module Build.Locations.Kitchen.FloorArea.Actions.PlantPot.Get where
import Game.Model.World (GetAction (..))

getAction :: GetAction
getAction = GetAction
  {   _updateGet' = pass
    , _get' = const pass -- tryGetM plantPotGID standardGetM
  }
