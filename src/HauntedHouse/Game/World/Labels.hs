module HauntedHouse.Game.World.Labels where

import HauntedHouse.Tokenizer (Lexeme(..))

locationIDNames :: [(String,Lexeme)]
locationIDNames = [("kitchen", KITCHEN),("hall", HALL)]

numberOfLocations :: [Integer]
numberOfLocations = [1 .. (toInteger $ length locationIDNames)]

locationNames :: [String]
locationNames = map fst locationIDNames

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