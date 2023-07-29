module HauntedHouse.Game.Build.Exits where

import Data.HashSet qualified
import HauntedHouse.Tokenizer (directions, Lexeme)
import Data.Char qualified (toLower, toUpper)
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

directionName :: Lexeme -> String
directionName l = map Data.Char.toLower $ toString l

directionNames :: [String]
directionNames = map directionName directions' 

directionLabelValuePair :: [(String,Lexeme)]
directionLabelValuePair = zip directionNames directions' 

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
