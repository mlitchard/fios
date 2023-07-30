module HauntedHouse.Build.ExitTemplate where

import HauntedHouse.Build.Exits
import HauntedHouse.Build.Template
import HauntedHouse.Game.Model.GID
import HauntedHouse.Game.Model.World

-- foldMapM (uncurry objectGIDDeclaration) objectIntPairs
foldMapM (uncurry (gidDeclaration "Exit") ) exitIntPairs