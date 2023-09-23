{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Build.ObjectTemplate where 

import Game.Model.GID ( GID(GID) ) 
import Game.Model.World (Object)
import Build.ObjectLabels (objectIntPairs)
import Build.Template ( gidDeclaration )

foldMapM (uncurry (gidDeclaration "Object")) objectIntPairs

