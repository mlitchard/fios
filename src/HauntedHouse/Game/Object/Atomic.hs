module HauntedHouse.Game.Object.Atomic where 
import HauntedHouse.Tokenizer (Lexeme)
import HauntedHouse.Game.GID (GID)

newtype ObjectLabel 
  = ObjectLabel {_unObjectLabel :: Lexeme} deriving stock (Eq,Ord,Show)

instance ToText ObjectLabel where
  toText :: ObjectLabel -> Text
  toText (ObjectLabel oname) = toText oname

newtype Descriptor = Descriptor Lexeme deriving stock (Eq,Ord,Show)

newtype DescriptorMap 
  = DescriptorMap (Map Descriptor (NonEmpty (GID ObjectLabel))) 
      deriving stock (Eq,Ord,Show)