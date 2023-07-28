module HauntedHouse.Game.Build.LocationLabels where 

import HauntedHouse.Tokenizer (Lexeme (..))
import HauntedHouse.Game.Model.Mapping  (Label (..))
import HauntedHouse.Game.Model.World    (Location)

kitchenLabel :: Label Location 
kitchenLabel = Label KITCHEN 

hallLabel :: Label Location 
hallLabel = Label HALL