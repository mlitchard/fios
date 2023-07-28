module HauntedHouse.Game.Model.Mapping where

import Data.Map.Strict qualified 
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Tokenizer (Lexeme)

newtype Label a = Label {_unLabel :: Lexeme} deriving (Show,Eq,Ord)
newtype Mapping a = Mapping {_unMapping :: Data.Map.Strict.Map (GID a) a}
                      deriving stock (Show,Eq,Ord)