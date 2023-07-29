module HauntedHouse.Game.Build.ExitTemplate where

import HauntedHouse.Game.Build.Exits
import HauntedHouse.Game.Build.Template
import HauntedHouse.Game.Model.GID
import HauntedHouse.Game.Model.World

-- foldMapM (uncurry objectGIDDeclaration) objectIntPairs
foldMapM (uncurry (gidDeclaration "Exit") ) exitIntPairs