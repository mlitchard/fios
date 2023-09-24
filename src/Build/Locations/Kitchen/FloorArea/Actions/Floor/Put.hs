module Build.Locations.Kitchen.FloorArea.Actions.Floor.Put where 
import Game.Model.World (PutAction (..))

putAction :: PutAction 
putAction = PutAction {
    _updatePut' = pass 
  , _put' = const (const pass) 
}