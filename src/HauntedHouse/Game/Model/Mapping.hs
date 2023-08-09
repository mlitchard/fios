module HauntedHouse.Game.Model.Mapping where

import Data.Map.Strict qualified 
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Tokenizer (Lexeme)
import qualified Data.List.NonEmpty

-- ObjectLabel Map (Label Object) [GID Object]
-- LocationLabel Map (Label Location) [GID Label]
-- ExitMap Map (GID Exit) (GID Location)
-- ObjectData Map (GID Object) Object
-- LocationData Map (GID Label) Label 
newtype Label a = Label {_unLabel' :: Lexeme} deriving stock (Show,Eq,Ord)

newtype ContainerMap a = ContainerMap 
  { _unContainerMap' :: Map (Label a) (Data.List.NonEmpty.NonEmpty (GID a))} 
    deriving stock (Eq,Ord,Show)
  
newtype NeighborMap a b = NeighborMap {
  _unNeighborMap :: Data.Map.Strict.Map a (Data.List.NonEmpty.NonEmpty (GID b))
} deriving stock Show 

newtype LabelToGIDListMapping a 
  = LabelToGIDListMapping 
    { _unLabelToGIDListMapping' :: Data.Map.Strict.Map (Label a) (NonEmpty (GID a))}
      deriving stock Show 

newtype LabelToGIDMapping a b
  = LabelToGIDMapping
      { _unLabelToGIDMapping' :: Data.Map.Strict.Map (Label a) (GID b)}
        deriving stock Show 

newtype GIDToDataMapping a 
  = GIDToDataMapping {_unGIDToDataMapping' :: Data.Map.Strict.Map (GID a) a} 
      deriving stock Show

newtype GIDToGIDMapping a b 
  = GIDToGIDMapping 
    {_unGIDtoGIDMapping' :: Data.Map.Strict.Map (GID a ) (GID b)}
        deriving stock Show 