{-# OPTIONS_GHC -Wno-missing-signatures #-}
module HauntedHouse.Build.LocationTemplate where 

import HauntedHouse.Build.Locations
import HauntedHouse.Build.Template (gidDeclaration)
import HauntedHouse.Game.Model.GID      (GID (..))
import HauntedHouse.Game.Model.World    (Location)

foldMapM (uncurry (gidDeclaration "Location")) locationIntPairs