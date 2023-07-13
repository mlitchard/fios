module HauntedHouse.Game.Location.Domain where

data LocationLabel
  = Attic
  | LivingRoom
  | Hall 
  | Kitchen
  deriving stock (Eq, Ord, Show)
