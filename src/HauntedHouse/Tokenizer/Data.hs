module HauntedHouse.Tokenizer.Data where

import Data.HashSet qualified as HS
import Data.Hashable (hashUsing)
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char (spaceChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (get)

type Parser = Parsec Void String

nouns :: HashSet Lexeme
nouns = locations <> objects <> directions
locations :: HashSet Lexeme
locations = 
  HS.fromList [PALACE, BASEMENT, ATTIC, MAZE, DEN, PARLOUR, VOIDLESSVOID]
objects :: HashSet Lexeme
objects = 
  HS.fromList [PLANT, POT, BOOK, BELL, CANDLE, TEA, DOOR, CABINET, SINK, SHELF
                , LEFT, RIGHT, FRONT, BACK]

-- https://stackoverflow.com/questions/44130436/how-to-have-a-sum-type-adt-with-a-known-set-of-string-literals

verbs :: HashSet Lexeme
verbs =
  HS.fromList ([LOOK, MOVE, PLANT, PUT, PLACE, OPEN, GO, CLOSE, GET] :: [Lexeme])

determiners :: HashSet Lexeme
determiners = HS.fromList ([THAT, THIS, THE, A, MY] :: [Lexeme])

prepositions :: HashSet Lexeme
prepositions =
  HS.fromList [TO, WITH, IN, WHEN, UNDER, OVER, ABOVE, AT, ON, OF, BEHIND, FROM]

adjectives :: HashSet Lexeme
adjectives = 
  HS.fromList [MIND, BLUE, RED, GREAT, LONG, OLD, DRUNK, PLANT, POT, TEA
              , CABINET, LOCKED, UNLOCKED, KITCHEN, VISIBLE, LEFT, RIGHT
              , FRONT, BEHIND]

directions :: HashSet Lexeme
directions = HS.fromList [NORTH, EAST, SOUTH, WEST, DOWN, UP]

numbers :: HashSet Lexeme
numbers = HS.fromList [ONE, TWO, THREE]

actors :: HashSet Lexeme
actors = HS.fromList [FORD, WILLIAM]

symbol :: String -> Parser String
symbol = L.symbol sc

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

data Lexeme
  = FROM
  | OPEN
  | CLOSE
  | SHUT
  | KITCHEN
  | CABINET
  | DOOR
  | ALL
  | EXAMINE
  | PALACE
  | BASEMENT
  | ATTIC
  | MAZE
  | DEN
  | PARLOUR
  | VOIDLESSVOID
  | PLANT
  | POT
  | BOOK
  | BELL
  | CANDLE
  | TEA
  | LOOK
  | MOVE
  | PUT
  | PLACE
  | THAT
  | THIS
  | THE
  | A
  | MY
  | ME
  | TO
  | WITH
  | IN
  | WHEN
  | UNDER
  | OVER
  | ABOVE
  | AT
  | ON
  | MIND
  | BLUE
  | RED
  | GREAT
  | LONG
  | OLD
  | DRUNK
  | FORD
  | WILLIAM
  | ONE
  | TWO
  | THREE
  | All
  | NORTH
  | EAST
  | SOUTH
  | WEST
  | DOWN
  | GET
  | SINK
  | THROUGH
  | SOIL
  | WATERING
  | CAN
  | BAG
  | UP
  | CLIMB
  | GO
  | HALL
  | MARQUEE
  | SHELF
  | LOCKED
  | UNLOCKED
  | VERBOSE
  | VISIBLE
  | LEFT
  | RIGHT
  | FRONT
  | BEHIND
  | OF
  | BACK
  | SEPERATOR
  deriving stock (Show, Eq, Enum, Ord)

instance ToText Lexeme where
  toText :: Lexeme -> Text
  toText txt = toText (show txt :: String)

instance ToString Lexeme where
  toString :: Lexeme -> String
  toString = toString . toText

instance Hashable Lexeme where
  hashWithSalt :: Int -> Lexeme -> Int
  hashWithSalt = hashUsing fromEnum

term :: Parser Lexeme
term = 
  PALACE <$ symbol "PALACE"
    <|> FROM <$ symbol "FROM"
    <|> BASEMENT <$ symbol "BASEMENT"
    <|> ATTIC <$ symbol "ATTIC"
    <|> MAZE <$ symbol "MAZE"
    <|> DEN <$ symbol "DEN"
    <|> PARLOUR <$ symbol "PARLOUR"
    <|> VOIDLESSVOID <$ symbol "VOIDLESSVOID"
    <|> PLANT <$ symbol "PLANT"
    <|> POT <$ symbol "POT"
    <|> BOOK <$ symbol "BOOK"
    <|> BELL <$ symbol "BELL"
    <|> CANDLE <$ symbol "CANDLE"
    <|> TEA <$ symbol "TEA"
    <|> LOOK <$ symbol "LOOK"
    <|> MOVE <$ symbol "MOVE"
    <|> PUT <$ symbol "PUT"
    <|> PLACE <$ symbol "PLACE"
    <|> THAT <$ symbol "THAT"
    <|> THIS <$ symbol "THIS"
    <|> OF <$ symbol "OF"
    <|> THE <$ symbol "THE"
    <|> AT <$ symbol "AT"
    <|> MY <$ symbol "MY"
    <|> ME <$ symbol "ME"
    <|> TO <$ symbol "TO"
    <|> WITH <$ symbol "WITH"
    <|> IN <$ symbol "IN"
    <|> WHEN <$ symbol "WHEN"
    <|> under 
    <|> OVER <$ symbol "OVER"
    <|> ON <$ symbol "ON"
    <|> MIND <$ symbol "MIND"
    <|> BLUE <$ symbol "BLUE"
    <|> RED <$ symbol "RED"
    <|> GREAT <$ symbol "GREAT"
    <|> LONG <$ symbol "LONG"
    <|> OLD <$ symbol "OLD"
    <|> DRUNK <$ symbol "DRUNK"
    <|> FORD <$ symbol "FORD"
    <|> WILLIAM <$ symbol "WILLIAM"
    <|> ONE <$ symbol "ONE"
    <|> TWO <$ symbol "TWO"
    <|> THREE <$ symbol "THREE"
    <|> ALL <$ symbol "ALL"
    <|> NORTH <$ symbol "NORTH"
    <|> EAST <$ symbol "EAST"
    <|> SOUTH <$ symbol "SOUTH"
    <|> WEST <$ symbol "WEST"
    <|> UP <$ symbol "UP"
    <|> DOWN <$ symbol "DOWN"
    <|> EXAMINE <$ symbol "EXAMINE"
    <|> get
    <|> OPEN <$ symbol "OPEN"
    <|> CABINET <$ symbol "CABINET"
    <|> DOOR <$ symbol "DOOR"
    <|> SINK <$ symbol "SINK"
    <|> KITCHEN <$ symbol "KITCHEN"
    <|> THROUGH <$ symbol "THROUGH"
    <|> SOIL <$ symbol "SOIL"
    <|> WATERING <$ symbol "WATERING"
    <|> CAN <$ symbol "CAN"
    <|> BAG <$ symbol "BAG"
    <|> GO <$ symbol "GO"
    <|> close 
    <|> CLIMB <$ symbol "CLIMB"
    <|> HALL <$ symbol "HALL"
    <|> MARQUEE <$ symbol "MARQUEE"
    <|> SHELF <$ symbol "SHELF"
    <|> LOCKED <$ symbol "LOCKED"
    <|> UNLOCKED <$ symbol "UNLOCKED"
    <|> VERBOSE <$ symbol "VERBOSE"
    <|> VISIBLE <$ symbol "VISIBLE"
    <|> LEFT <$ symbol "LEFT"
    <|> RIGHT <$ symbol "RIGHT"
    <|> FRONT <$ symbol "FRONT"
    <|> BEHIND <$ symbol "BEHIND"
    <|> SINK <$ symbol "SINK"
    <|> BACK <$ symbol "BACK"
    <|> ABOVE <$ symbol "ABOVE"
    <|> A <$ symbol "A"
    <|> SEPERATOR <$ symbol ","

under :: Parser Lexeme
under = UNDER <$ symbol "UNDER" <|> UNDER <$ symbol "BELOW"

get :: Parser Lexeme 
get = GET <$ symbol "GET" <|> GET <$ symbol "TAKE"

close :: Parser Lexeme
close = CLOSE <$ symbol "CLOSE" <|> CLOSE <$ symbol "SHUT"