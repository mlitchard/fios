module HauntedHouse.Game.Location.Domain where
import HauntedHouse.Tokenizer (Lexeme)

newtype LocationLabel = LocationLabel {_unLocationLabel :: Lexeme}
  deriving stock (Eq, Ord, Show)
