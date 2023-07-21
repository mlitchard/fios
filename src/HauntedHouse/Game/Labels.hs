module HauntedHouse.Game.Labels where 
import HauntedHouse.Tokenizer (Lexeme)

newtype ObjectLabel 
  = ObjectLabel {_unObjectLabel :: Lexeme} deriving stock (Eq,Ord,Show)

instance ToText ObjectLabel where
  toText :: ObjectLabel -> Text
  toText (ObjectLabel oname) = toText oname

newtype AgentLabel 
          = AgentLabel {_unAgentLabel :: Lexeme} 
              deriving stock (Eq,Ord,Show)

newtype LocationLabel 
          = LocationLabel {_unLocationLabel :: Lexeme} 
              deriving stock (Eq,Ord,Show)

newtype ExitLabel
          = ExitLabel {_unExitLabel :: Lexeme} deriving stock (Eq,Ord,Show)