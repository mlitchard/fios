module HauntedHouse.Game.World.Objects where

import HauntedHouse.Game.GID (GID (..))
import HauntedHouse.Game.Object.Atomic (ObjectLabel)
import HauntedHouse.Game.World.InitState
import HauntedHouse.Game.Object
    ( Object, ObjectLabelMap(ObjectLabelMap, _unObjectLabelMap) )
import qualified Data.Map.Strict (lookup, insert)
import Control.Monad.Except (MonadError(throwError))
import qualified Data.List.NonEmpty
import qualified Data.List.NonEmpty as Data.NonEmpty.List

initOrg :: NonEmpty (ObjectLabel,GID ObjectLabel)
            -> (ObjectLabel,[GID ObjectLabel])
initOrg xs@((objLabel,_) :| _) = (,) objLabel $ map snd $ toList xs

{-

data InitState = InitState {
  _objects :: ObjectLabelMap 
  , _locations :: LocationMap
  , _world :: World
}

newtype ObjectLabelMap = ObjectLabelMap
  { _unObjectLabelMap :: Data.Map.Strict.Map ObjectLabel 
                                             (NonEmpty (GID Object))
  }

-}

popObjectGID :: ObjectLabel -> InitStateT (GID Object)
popObjectGID oLabel = do
  omap' <- _unObjectLabelMap . _objectLabelMap' <$> get
  let mGid = Data.Map.Strict.lookup oLabel omap'
  case mGid of
    Nothing -> throwError ("could not find " <> show oLabel)
    Just (x :| xs) -> Data.NonEmpty.List.fromList xs
                        & (\xs' -> modify (`updateObject` xs'))
                        >> pure x
  where
    updateObject :: InitState -> Data.List.NonEmpty.NonEmpty (GID Object) -> InitState
    updateObject init'@(InitState (ObjectLabelMap objects) _ _ _) xs =
      let updatedMap = ObjectLabelMap $ Data.Map.Strict.insert oLabel xs objects
      in init'{_objectLabelMap' = updatedMap}
{-
getObject :: GID Object -> InitStateT Object
getObject gid = 
-}