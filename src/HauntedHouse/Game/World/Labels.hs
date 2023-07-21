module HauntedHouse.Game.World.Labels where
import HauntedHouse.Game.Labels (ObjectLabel (..), LocationLabel (..))
import HauntedHouse.Tokenizer (Lexeme(..))

kitchenLabel :: LocationLabel 
kitchenLabel = LocationLabel KITCHEN 

kitchenCabinetAboveShelfLabel :: ObjectLabel 
kitchenCabinetAboveShelfLabel = ObjectLabel CABINET 

kitchenCabinetBelowShelfLabel :: ObjectLabel 
kitchenCabinetBelowShelfLabel = ObjectLabel CABINET

kitchenShelfLabel :: ObjectLabel
kitchenShelfLabel = ObjectLabel SHELF 

kitchenSinkCabinetAboveLabel :: ObjectLabel
kitchenSinkCabinetAboveLabel = ObjectLabel CABINET 

kitchenSinkCabinetBelowLabel :: ObjectLabel
kitchenSinkCabinetBelowLabel = ObjectLabel CABINET 

kitchenSinkLabel :: ObjectLabel
kitchenSinkLabel = ObjectLabel SINK 

hallLabel :: LocationLabel
hallLabel = LocationLabel HALL 