{-# OPTIONS_GHC -Wno-orphans #-}
module HauntedHouse.Game.Model.ToText where
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Model.GID (GID (_unGID'))

instance ToText (Label Object) where
  toText :: Label Object -> Text 
  toText = toText . _unLabel'

instance ToText (Label Location) where 
  toText :: Label Location -> Text
  toText = toText . _unLabel'

instance ToText (GID a) where 
  toText :: GID a -> Text 
  toText = show  . _unGID'