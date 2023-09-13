module HauntedHouse.Build.ObjectLabels where 

import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.World  (Object)
import HauntedHouse.Tokenizer         (Lexeme(..))

{-
The following are the keys for "Map (Label Object) 
                                  Data.List.NonEmpty (GID Object)""
-}
sink :: Label Object
sink = Label SINK 

shelf :: Label Object 
shelf = Label SHELF 

cabinet :: Label Object 
cabinet = Label CABINET

door :: Label Object 
door = Label DOOR 

plantPot :: Label Object 
plantPot = Label POT 

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
  , "plantPot"]

numberOfObjects :: [Integer]
numberOfObjects = [1 .. (toInteger $ length objectNames)]

objectIntPairs :: [(String,Integer)]
objectIntPairs = zip objectNames numberOfObjects