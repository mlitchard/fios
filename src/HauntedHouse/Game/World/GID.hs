{-# OPTIONS_GHC -Wno-missing-signatures #-}
module HauntedHouse.Game.World.GID where

import HauntedHouse.Game.World.Labels
import HauntedHouse.Game.World.Template
import HauntedHouse.Game.GID (GID(GID))

foldMapM (uncurry objectGIDDeclaration) $ zip numberOfObjects objectNames 
