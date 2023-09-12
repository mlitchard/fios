module HauntedHouse.Game.Engine.Utilities where
import HauntedHouse.Game.Model.Condition (Proximity (..))
import HauntedHouse.Tokenizer (Lexeme (..))
import HauntedHouse.Recognizer
import HauntedHouse.Game.Model.World
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
data PrepPhrase
  = PrepPhrase1 Preposition NounPhrase
  | PrepPhrase2 Preposition  Determiner AdjPhrase NounPhrase
  -- | Preposition Preposition
  deriving stock (Show, Eq, Ord)
-}
prepositionFromPhrase :: PrepPhrase -> Preposition 
prepositionFromPhrase (PrepPhrase1 prep _)      = prep 
prepositionFromPhrase (PrepPhrase2 prep _ _ _)  = prep

