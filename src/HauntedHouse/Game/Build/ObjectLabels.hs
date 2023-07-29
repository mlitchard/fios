module HauntedHouse.Game.Build.ObjectLabels where 

import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.World  (Object)
import HauntedHouse.Tokenizer         (Lexeme(SINK,SHELF, CABINET))

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
  , "kitchenCabinetBelowShelf"]

numberOfObjects :: [Integer]
numberOfObjects = [1 .. (toInteger $ length objectNames)]

objectIntPairs :: [(String,Integer)]
objectIntPairs = zip objectNames numberOfObjects