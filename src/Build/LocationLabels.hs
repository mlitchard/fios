module Build.LocationLabels where 

import Tokenizer (Lexeme (..))
import Game.Model.Mapping  (Label (..))
import Game.Model.World    (Location)

kitchenLocationLabel :: Label Location 
kitchenLocationLabel = Label KITCHEN 

hallLocationLabel :: Label Location 
hallLocationLabel = Label HALL