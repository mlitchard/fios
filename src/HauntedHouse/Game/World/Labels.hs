module HauntedHouse.Game.World.Labels where
import HauntedHouse.Game.Labels (LocationLabel (..))
import HauntedHouse.Tokenizer (Lexeme(..))

-- objectLabelDeclaration "kitchenSinkCabinetBelow" CABINET
{-
kitchenLabel :: LocationLabel 
kitchenLabel = LocationLabel KITCHEN 
-}

-- kitchenCabinetAboveShelfLabel :: ObjectLabel
-- kitchenCabinetAboveShelfLabel = ObjectLabel CABINET

-- kitchenCabinetBelowShelfLabel :: ObjectLabel 
-- kitchenCabinetBelowShelfLabel = ObjectLabel CABINET

-- kitchenShelfLabel :: ObjectLabel
-- kitchenShelfLabel = ObjectLabel SHELF

-- kitchenSinkCabinetAboveLabel :: ObjectLabel
-- kitchenSinkCabinetAboveLabel = ObjectLabel CABINET

-- kitchenSinkCabinetBelowLabel :: ObjectLabel
-- kitchenSinkCabinetBelowLabel = ObjectLabel CABINET 

-- kitchenSinkLabel :: ObjectLabel
-- kitchenSinkLabel = ObjectLabel SINK

hallLabel :: LocationLabel
hallLabel = LocationLabel HALL
objectIDNames :: [(String,Lexeme)]
objectIDNames =
  [("kitchenSink", SINK)
  ,("kitchenCabinetBelowSink", CABINET)
  , ("kitchenCabinetAboveSink", CABINET)
  , ("kitchenShelf", SHELF)
  , ("kitchenCabinetAboveShelf",CABINET)
  , ("kitchenCabinetBelowShelf",CABINET)]

numberOfObjects :: [Integer]
numberOfObjects = [1 .. (toInteger $ length objectIDNames)]

objectNames :: [String]
objectNames = map fst objectIDNames