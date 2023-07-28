module HauntedHouse.Game.Build.Labels where

import HauntedHouse.Tokenizer (Lexeme(..))

locationIDNames :: [(String,Lexeme)]
locationIDNames = [("kitchen", KITCHEN),("hall", HALL)]

numberOfLocations :: [Integer]
numberOfLocations = [1 .. (toInteger $ length locationNames)]

locationIntPairs :: [(String,Integer)]
locationIntPairs = zip locationNames numberOfLocations 

locationNames :: [String]
locationNames = map fst locationIDNames

{-
objectIDNames :: [(String,Lexeme)]
objectIDNames =
  [("kitchenSink", SINK)
  ,("kitchenCabinetBelowSink", CABINET)
  , ("kitchenCabinetAboveSink", CABINET)
  , ("kitchenShelf", SHELF)
  , ("kitchenCabinetAboveShelf",CABINET)
  , ("kitchenCabinetBelowShelf",CABINET)]
-}
sink :: Label Object 
sink = Label SINK 

shelf 
numberOfObjects :: [Integer]
numberOfObjects = [1 .. (toInteger $ length objectIDNames)]

objectNames :: [String]
objectNames = map fst objectIDNames

objectIntPairs :: [(String,Integer)]
objectIntPairs = zip objectNames numberOfObjects