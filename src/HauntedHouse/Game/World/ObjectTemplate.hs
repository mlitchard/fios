module HauntedHouse.Game.World.ObjectTemplate where 

import Language.Haskell.TH
import HauntedHouse.Game.GID 
import HauntedHouse.Game.World.Objects
import HauntedHouse.Game.World.Template
import HauntedHouse.Game.Labels (ObjectLabel (..))
import HauntedHouse.Tokenizer (Lexeme(..))

foldMapM (uncurry objectGIDDeclaration) $ zip numberOfObjects objectNames 