module HauntedHouse.Game.Object (
 module HauntedHouse.Game.Object.Container 
, module HauntedHouse.Game.Object.Domain 

)where

import HauntedHouse.Game.Object.Container

import HauntedHouse.Game.Object.Domain
{-
makeObjectMap :: [ObjectLabel] -> ObjectLabelMap 
makeObjectMap xs = ObjectLabelMap $ 
  zipWith (\idx oLabel -> (GID idx, oLabel)) gidRange xs 
  where
    gidRange = fromList [1 .. (length xs)]      
-}