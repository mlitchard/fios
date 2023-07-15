module HauntedHouse.Game.World.Labels where
import HauntedHouse.Game.Location (LocationLabel (..))
import HauntedHouse.Game.Object.Atomic (ObjectLabel (..))
import HauntedHouse.Tokenizer (Lexeme(..))

kitchenLabel :: LocationLabel
kitchenLabel = LocationLabel KITCHEN 

hallLabel :: LocationLabel
hallLabel    = LocationLabel HALL 

kitchenCabinetAboveShelfLabel :: ObjectLabel
kitchenCabinetAboveShelfLabel = ObjectLabel CABINET 

kitchenCabinetBelowShelfLabel :: ObjectLabel
kitchenCabinetBelowShelfLabel = ObjectLabel CABINET 

kitchenShelfLabel :: ObjectLabel
kitchenShelfLabel = ObjectLabel SHELF 

kitchenSinkLabel :: ObjectLabel
kitchenSinkLabel = ObjectLabel SINK

kitchenSinkCabinetAboveLabel :: ObjectLabel
kitchenSinkCabinetAboveLabel = ObjectLabel CABINET

kitchenSinkCabinetBelowLabel :: ObjectLabel
kitchenSinkCabinetBelowLabel = ObjectLabel CABINET

kitchenSinkName :: ObjectLabel
kitchenSinkName = ObjectLabel SINK

