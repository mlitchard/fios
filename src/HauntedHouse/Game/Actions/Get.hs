module HauntedHouse.Game.Actions.Get where
import HauntedHouse.Game.Model.World
import Control.Monad.Except (MonadError(..))
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Object (getObjectM)
import qualified Data.Map.Strict
import qualified Data.List.NonEmpty

noGetM :: GameStateExceptT ()
noGetM = throwError "Are you serious?"

standardGetM :: GID Object -> GameStateExceptT ()
standardGetM gid = do
  gidList <- concatMap Data.List.NonEmpty.toList . toElems
                <$> (getLocationIdM >>= getLocationM)
  if gid `elem` gidList
    then print "You would check for visiblity here"
    else throwError "You don't see that here."
  pass
  where
    toElems = Data.Map.Strict.elems . _unLabelToGIDListMapping' . _objectLabelMap'