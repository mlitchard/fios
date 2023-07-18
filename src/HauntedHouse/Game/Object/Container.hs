module HauntedHouse.Game.Object.Container where

import Data.Map.Strict qualified
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Object.Container.Domain (Placeability (..)
                                                  , RelatedObjects (..))
import HauntedHouse.Game.Labels (ObjectLabel)

{-
data ContainerState
  = ContainedIn ContainedIn
  | Containing (Maybe Container) -- Nothing means not a container
  deriving stock (Show)
-}
-- Nothing means not a container or shelf

-- newtype RelatedObjects 
--          = RelatedObjects (Map Placeability [GID ObjectLabel]) 
--              deriving stock Show
-- update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
update :: (Placeability, GID ObjectLabel) -> RelatedObjects -> RelatedObjects
update (p,obj) (RelatedObjects robjs ) = 
  RelatedObjects $ Data.Map.Strict.update update' p robjs  
  where
    update' :: [GID ObjectLabel] -> Maybe [GID ObjectLabel]
    update' xs = Just (obj : xs)