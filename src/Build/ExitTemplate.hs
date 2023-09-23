module Build.ExitTemplate where

import Build.Exits
import Build.Template
import Game.Model.GID
import Game.Model.World

-- foldMapM (uncurry objectGIDDeclaration) objectIntPairs
foldMapM (uncurry (gidDeclaration "Exit") ) exitIntPairs