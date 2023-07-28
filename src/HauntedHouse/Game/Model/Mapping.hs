module HauntedHouse.Game.Model.Mapping where

import Data.Map.Strict qualified 
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Tokenizer (Lexeme)

-- ObjectLabel Map (Label Object) [GID Object]
-- LocationLabel Map (Label Location) [GID Label]
-- ExitMap Map (GID Exit) (GID Location)
-- ObjectData Map (GID Object) Object
-- LocationData Map (GID Label) Label 
newtype Label a = Label {_unLabel :: Lexeme} deriving (Show,Eq,Ord)

newtype LabelToGIDMapping a 
  = LabelToGIDMapping 
    { _unLabelMapping :: Data.Map.Strict.Map (Label a) (NonEmpty (GID a))}
    
newtype GIDToDataMapping a 
  = GIDToDataMapping {_unGIDMapping :: Data.Map.Strict.Map (GID a) a}

newtype GIDToGIDMapping a b 
  = GIDToGIDMapping 
    {_unGIDtoGIDMapping :: Data.Map.Strict.Map (GID a ) (GID b)}
