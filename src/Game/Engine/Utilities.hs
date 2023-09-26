module Game.Engine.Utilities where
import Game.Model.Condition (Proximity (..))
import Tokenizer (Lexeme (..))
import Recognizer
import Game.Model.World
import Game.Model.Mapping

descriptiveLabel :: Lexeme -> Label Adjective
descriptiveLabel = Label

directObjectLabel :: Lexeme -> Label Object
directObjectLabel = Label

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

