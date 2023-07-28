module HauntedHouse.Game.Build.LocationTemplate where 

import HauntedHouse.Tokenizer 
import HauntedHouse.Game.Build.Labels
import HauntedHouse.Game.Build.Template
import HauntedHouse.Game.Model.GID      (GID (..))
import HauntedHouse.Game.Model.Mapping  (Label (..))
import HauntedHouse.Game.Model.World    (Object, Location)

foldMapM (uncurry locationGIDDeclaration) locationIntPairs