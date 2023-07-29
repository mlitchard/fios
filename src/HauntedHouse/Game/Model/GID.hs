module HauntedHouse.Game.Model.GID where

newtype GID a = GID {_unGID' :: Int}
  deriving stock (Show, Ord, Eq)

