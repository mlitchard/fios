module HauntedHouse.Game.Object.Atomic where 
import HauntedHouse.Tokenizer (Lexeme)
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Labels (ObjectLabel)

newtype Descriptor = Descriptor Lexeme deriving stock (Eq,Ord,Show)

newtype DescriptorMap 
  = DescriptorMap (Map Descriptor (NonEmpty (GID ObjectLabel))) 
      deriving stock (Eq,Ord,Show)