{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Build.LocationTemplate where 

import Build.Locations
import Build.Template (gidDeclaration)
import Game.Model.GID      (GID (..))
import Game.Model.World    (Location)

foldMapM (uncurry (gidDeclaration "Location")) locationIntPairs