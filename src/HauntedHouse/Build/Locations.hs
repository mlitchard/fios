module HauntedHouse.Build.Locations where

import HauntedHouse.Tokenizer (Lexeme(..))

locationIDNames :: [(String,Lexeme)]
locationIDNames = [("kitchen", KITCHEN),("hall", HALL)]

numberOfLocations :: [Integer]
numberOfLocations = [1 .. (toInteger $ length locationNames)]

locationIntPairs :: [(String,Integer)]
locationIntPairs = zip locationNames numberOfLocations 

locationNames :: [String]
locationNames = map fst locationIDNames
