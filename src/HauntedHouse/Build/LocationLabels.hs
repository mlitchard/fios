module HauntedHouse.Build.LocationLabels where 

import HauntedHouse.Tokenizer (Lexeme (..))
import HauntedHouse.Game.Model.Mapping  (Label (..))
import HauntedHouse.Game.Model.World    (Location)

kitchenLocationLabel :: Label Location 
kitchenLocationLabel = Label KITCHEN 

hallLocationLabel :: Label Location 
hallLocationLabel = Label HALL