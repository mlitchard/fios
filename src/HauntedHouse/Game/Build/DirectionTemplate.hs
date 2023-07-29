module HauntedHouse.Game.Build.DirectionTemplate where 

import HauntedHouse.Game.Build.Exits
import HauntedHouse.Game.Build.Template
import HauntedHouse.Game.Model.Mapping (Label (..))
import HauntedHouse.Game.Model.World (Exit)
import HauntedHouse.Tokenizer.Data ( Lexeme (..) )

{-

labelTemplate :: String -> String -> Lexeme -> DecsQ
labelTemplate typeStr binding lexeme =

-}
foldMapM (uncurry (labelTemplate "Exit")) directionLabelValuePair
-- directionLabelValuePair