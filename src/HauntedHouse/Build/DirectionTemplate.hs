module HauntedHouse.Build.DirectionTemplate where 

import HauntedHouse.Build.Exits
import HauntedHouse.Build.Template ( labelTemplate )
import HauntedHouse.Game.Model.Mapping (Label (..))
import HauntedHouse.Game.Model.World (Exit)
import HauntedHouse.Tokenizer.Data ( Lexeme (..) )

foldMapM (uncurry (labelTemplate "Exit")) directionLabelValuePair
-- directionLabelValuePair