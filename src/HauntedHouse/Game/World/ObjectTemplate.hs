module HauntedHouse.Game.World.ObjectTemplate where 

import HauntedHouse.Game.World.Objects
import HauntedHouse.Game.World.Template
import Language.Haskell.TH
import HauntedHouse.Game.Labels (ObjectLabel (..))
import HauntedHouse.Tokenizer (Lexeme(..))

foldMapM  @[Dec] (uncurry objectLabelDeclaration) objectIDNames