module HauntedHouse.Game.Build.Exits where 

import Data.HashSet qualified
import HauntedHouse.Tokenizer (directions, Lexeme)
{-
objectNames :: [String] 
objectNames = 
  ["kitchenSink"
  ,"kitchenCabinetBelowSink"
  , "kitchenCabinetAboveSink"
  , "kitchenShelf"
  , "kitchenCabinetAboveShelf"
  , "kitchenCabinetBelowShelf"]
-} 

directions' :: [Lexeme]
directions' = Data.HashSet.toList directions 

numberOfDirections :: Int 
numberOfDirections = length directions' 

exitNames :: [String]
exitNames = kitchenExits <> hallExits

exitRange :: [Integer]
exitRange = [1 .. (toInteger .length $ exitNames)]

exitIntPairs :: [(String, Integer)]
exitIntPairs = zip exitNames exitRange 

kitchenExits :: [String]
kitchenExits = ["kitchenEastExit"]

hallExits :: [String]
hallExits = ["hallWestExit"]

numberOfExits :: Int
numberOfExits = length exitNames 
