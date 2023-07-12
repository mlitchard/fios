module HauntedHouse.Game.Location.Exits where

import Data.Map.Strict qualified (Map)
import HauntedHouse.Game.Location.Domain (LocationName)
import HauntedHouse.Tokenizer ( Lexeme ) 

newtype ExitMap = ExitMap 
  { _unExitMap :: Data.Map.Strict.Map ExitName LocationName}
   deriving stock Show

newtype ExitName = ExitName Lexeme deriving stock (Eq, Ord, Show)