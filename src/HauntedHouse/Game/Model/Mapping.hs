module HauntedHouse.Game.Model.Mapping where

import Data.Map.Strict qualified 
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Tokenizer (Lexeme)
import qualified Data.List.NonEmpty



newtype ContainerMap a = ContainerMap 
  { _unContainerMap' :: Map (Label a) (GIDList a)} 
    deriving stock (Eq,Ord,Show)

newtype ExitGIDMap exit object
  = ExitGIDMap {_unExitGIDMap' :: LabelToGIDMapping exit object}
      deriving stock Show

newtype ExitGIDDataMap exit = ExitGIDDataMap {_unExitGIDDataMap' :: GIDToDataMapping exit}
type GIDList a = (Data.List.NonEmpty.NonEmpty (GID a))

newtype GIDToDataMapping a 
  = GIDToDataMapping {_unGIDToDataMapping' :: Data.Map.Strict.Map (GID a) a} 
      deriving stock Show

newtype GIDToGIDMapping a b 
  = GIDToGIDMapping 
    {_unGIDtoGIDMapping' :: Data.Map.Strict.Map (GID a ) (GID b)}
        deriving stock Show 

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

newtype NeighborMap a b = NeighborMap {
  _unNeighborMap :: Data.Map.Strict.Map a (GIDList b)
} deriving stock Show 
