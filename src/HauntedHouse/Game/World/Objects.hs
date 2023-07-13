module HauntedHouse.Game.World.Objects where

makeMaps' ::  NonEmpty (ObjectName,Object) -> NonEmpty (ObjectName,GID Object)
makeMaps' xs = foldMap ((i,(on,_)) -> (on,GID i)) $ zip range' xs
  where range' = [1 .. (length xs)] 