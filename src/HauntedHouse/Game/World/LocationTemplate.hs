{-# OPTIONS_GHC -Wno-missing-signatures #-}
module HauntedHouse.Game.World.LocationTemplate where

import HauntedHouse.Game.World.Labels 
import HauntedHouse.Game.World.Template
import HauntedHouse.Game.Model.GID (GID(..))

foldMapM (uncurry locationGIDDeclaration) $ zip numberOfLocations locationNames