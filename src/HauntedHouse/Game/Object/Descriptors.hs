module HauntedHouse.Game.Object.Descriptors where
import HauntedHouse.Tokenizer (Lexeme)
import HauntedHouse.Game.GID (GID)

newtype Descriptor = Descriptor Lexeme deriving stock (Eq,Ord,Show)

newtype DescriptorMap 
  = DescriptorMap (Map Descriptor (NonEmpty (GID Descriptor))) 
      deriving stock (Eq,Ord,Show)