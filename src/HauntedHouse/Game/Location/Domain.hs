module HauntedHouse.Game.Location.Domain where 

data LocationName
    = Attic
    | LivingRoom
    | Kitchen
       deriving stock (Eq,Ord, Show)