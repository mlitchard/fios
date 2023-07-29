{-# OPTIONS_GHC -Wno-missing-signatures #-}
module HauntedHouse.Game.Build.ObjectTemplate where 

import HauntedHouse.Game.Model.GID ( GID(GID) ) 
import HauntedHouse.Game.Model.World (Object)
import HauntedHouse.Game.Build.ObjectLabels (objectIntPairs)
import HauntedHouse.Game.Build.Template ( gidDeclaration )

foldMapM (uncurry (gidDeclaration "Object")) objectIntPairs

