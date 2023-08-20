module HauntedHouse.Game.Model.GID where

newtype GID a = GID {_unGID' :: Int}
  deriving stock (Show, Ord, Eq)

instance ToText (GID a) where
  toText :: GID a -> Text
  toText = show  . _unGID'
