{-# OPTIONS_GHC -Wno-orphans #-}
module HauntedHouse.Game.Model.ToText where
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Model.GID (GID (_unGID'))
{-

newtype LabelToGIDMapping a 
  = LabelToGIDMapping 
    { _unLabelMapping :: Data.Map.Strict.Map (Label a) (NonEmpty (GID a))}
-}
{-
instance ToText Object where

  toText :: Object -> Text 
  toText = show . _unObject'
  -}
 {-
data ContainedBy a b
  = ByObject (GID a)
  | ByLocation (GID b)
  | ByPlayer 
  -}

instance ToText ContainedBy where 
  toText (ByObject gid)   = toText gid 
  toText (ByLocation gid) = toText gid
  toText ByPlayer         = "By Player"  

instance ToText (Label Object) where
  toText :: Label Object -> Text 
  toText = toText . _unLabel

instance ToText (Label Location) where 
  toText :: Label Location -> Text
  toText = toText . _unLabel

instance ToText (GID a) where 
  toText :: GID a -> Text 
  toText = show  . _unGID'

-- class  (Eq a) => Ord a  where

class (ToText b) => FormatMap a b where

  format :: a b -> [Text] 

-- instance FormatMap LabelToGIDMapping Object where 



