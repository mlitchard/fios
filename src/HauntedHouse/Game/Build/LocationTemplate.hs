{-# OPTIONS_GHC -Wno-missing-signatures #-}
module HauntedHouse.Game.Build.LocationTemplate where 

import HauntedHouse.Game.Build.Locations
import HauntedHouse.Game.Build.Template (gidDeclaration)
import HauntedHouse.Game.Model.GID      (GID (..))
import HauntedHouse.Game.Model.World    (Location)

foldMapM (uncurry (gidDeclaration "Location")) locationIntPairs