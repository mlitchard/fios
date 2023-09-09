module HauntedHouse.Game.Engine.Utilities where
import HauntedHouse.Game.Model.Condition (Proximity (..))
import HauntedHouse.Tokenizer (Lexeme (..))
import HauntedHouse.Recognizer

{-
data Proximity
  = PlacedOn
  | PlacedUnder
  | PlacedAbove
  | PlacedLeft
  | PlacedRight
  | PlacedFront 
  | PlacedBack
      deriving stock (Eq,Ord,Show)
-}
prepToProximity :: Lexeme -> Maybe Proximity 
prepToProximity ON = Just PlacedOn
prepToProximity UNDER = Just PlacedUnder
prepToProximity ABOVE = Just PlacedAbove
prepToProximity LEFT = Just PlacedLeft 
prepToProximity RIGHT = Just PlacedRight
prepToProximity FRONT = Just PlacedFront 
prepToProximity BEHIND = Just PlacedBehind 
prepToProximity _ = Nothing   

{-
data NounPhrase
  = NounPhrase1 Determiner NounPhrase
  | NounPhrase3 Number NounPhrase
  | Noun Noun
  deriving stock (Show, Eq, Ord)
-}
