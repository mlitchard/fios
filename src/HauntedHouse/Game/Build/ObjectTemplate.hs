module HauntedHouse.Game.Build.ObjectTemplate where 

import Language.Haskell.TH
import HauntedHouse.Game.Model.GID 
import HauntedHouse.Game.Build.Labels (objectIntPairs)
import HauntedHouse.Game.Build.Template
import HauntedHouse.Game.Model.Mapping (Label (..))

import HauntedHouse.Tokenizer (Lexeme(..))

foldMapM (uncurry objectGIDDeclaration) objectIntPairs
