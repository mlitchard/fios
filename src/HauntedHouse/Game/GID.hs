{-# LANGUAGE InstanceSigs #-}

module HauntedHouse.Game.GID where

newtype GID a = GID {unGID :: Int}
  deriving stock (Show, Ord, Eq)

instance ToText (GID a) where
  toText :: GID a -> Text
  toText = show . unGID
