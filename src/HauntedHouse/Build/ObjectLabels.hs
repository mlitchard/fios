module HauntedHouse.Build.ObjectLabels where 

import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.World  (Object)
import HauntedHouse.Tokenizer         (Lexeme(..))

{-
The following are the keys for "Map (Label Object) 
                                  Data.List.NonEmpty (GID Object)""
-}
kitchenSinkLabel :: Label Object
kitchenSinkLabel = Label SINK 

kitchenShelfLabel :: Label Object 
kitchenShelfLabel = Label SHELF 

cabinetLabel :: Label Object 
cabinetLabel = Label CABINET

doorLabel :: Label Object 
doorLabel = Label DOOR 

plantPotLabel :: Label Object 
plantPotLabel = Label POT 

floorLabel :: Label Object 
floorLabel = Label FLOOR
{-
These generate the Labels for the specific objects
-}
objectNames :: [String] 
objectNames = 
  ["kitchenSink"
  ,"kitchenCabinetBelowSink"
  , "kitchenCabinetAboveSink"
  , "kitchenShelf"
  , "kitchenCabinetAboveShelf"
  , "kitchenCabinetBelowShelf"
  , "kitchenEastDoor"
  , "kitchenEastPortal"
  , "kitchenFloor"
  , "plantPot"
  , "bagOfSoil"]

numberOfObjects :: [Integer]
numberOfObjects = [1 .. (toInteger $ length objectNames)]

objectIntPairs :: [(String,Integer)]
objectIntPairs = zip objectNames numberOfObjects