{-# OPTIONS_GHC -Wno-missing-signatures #-}
module HauntedHouse.Build.ObjectTemplate where 

import HauntedHouse.Game.Model.GID ( GID(GID) ) 
import HauntedHouse.Game.Model.World (Object)
import HauntedHouse.Build.ObjectLabels (objectIntPairs)
import HauntedHouse.Build.Template ( gidDeclaration )

foldMapM (uncurry (gidDeclaration "Object")) objectIntPairs

