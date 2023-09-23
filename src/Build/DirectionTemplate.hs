module Build.DirectionTemplate where 

import Build.Exits
import Build.Template ( labelTemplate )
import Game.Model.Mapping (Label (..))
import Game.Model.World (Exit)
import Tokenizer.Data ( Lexeme (..) )

foldMapM (uncurry (labelTemplate "Exit")) directionLabelValuePair
-- directionLabelValuePair