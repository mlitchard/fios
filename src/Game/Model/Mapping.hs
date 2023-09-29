module Game.Model.Mapping where

import Data.Map.Strict qualified 
import Game.Model.GID (GID)
import Tokenizer (Lexeme)

newtype ContainerMap a = ContainerMap 
  { _unContainerMap' :: Map (Label a) (GIDList a)} 
    deriving stock (Eq,Ord,Show)

newtype GIDToDataMap a b
  = GIDToDataMap {_unGIDToDataMap' :: Data.Map.Strict.Map (GID a) b} 
      deriving stock Show

newtype GIDToGIDMapping a b 
  = GIDToGIDMapping 
    {_unGIDtoGIDMapping' :: Data.Map.Strict.Map (GID a ) (GID b)}
        deriving stock Show 

type GIDList a = NonEmpty (GID a)

newtype Label a = Label {_unLabel' :: Lexeme} deriving stock (Show,Eq,Ord)

newtype LabelToGIDMapping a b
  = LabelToGIDMapping
      { _unLabelToGIDMapping' :: Data.Map.Strict.Map (Label a) (GID b)}
        deriving stock Show 

newtype LabelToGIDListMapping a b 
  = LabelToGIDListMapping 
    { _unLabelToGIDListMapping' :: Data.Map.Strict.Map (Label a) (GIDList b)}
      deriving stock Show 

newtype LocationObjectList a b 
  = LocationObjectList 
      {_unLocationObjectList :: Data.Map.Strict.Map a (GIDList b)}
        deriving stock Show

