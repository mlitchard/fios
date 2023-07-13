module HauntedHouse.Game.Location.Domain where

data LocationName
  = Attic
  | LivingRoom
  | Hall 
  | Kitchen
  deriving stock (Eq, Ord, Show)
