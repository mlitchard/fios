module HauntedHouse.Tokenizer.Data where

import Data.Hashable ( hashUsing ) 
import qualified Data.HashSet as HS
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (spaceChar)

type Parser = Parsec Void String

nouns :: HashSet Lexeme
nouns        = HS.fromList (locations <> objects)
locations :: [Lexeme]
locations      = [PALACE, BASEMENT,ATTIC,MAZE,DEN,PARLOUR,VOIDLESSVOID] :: [Lexeme]
objects :: [Lexeme]
objects        = [PLANT, POT, BOOK, BELL, CANDLE, TEA, DOOR, CABINET, SINK] :: [Lexeme]
-- https://stackoverflow.com/questions/44130436/how-to-have-a-sum-type-adt-with-a-known-set-of-string-literals

verbs :: HashSet Lexeme
verbs        =
  HS.fromList ([LOOK,MOVE,PLANT,PUT,PLACE, OPEN] :: [Lexeme])

determiners :: HashSet Lexeme
determiners  = HS.fromList ([THAT,THIS,THE, A,MY] :: [Lexeme])

prepositions :: HashSet Lexeme
prepositions =
  HS.fromList [TO,WITH,IN,WHEN,UNDER,OVER,ABOVE,AT,ON]

adjectives :: HashSet Lexeme
adjectives   = HS.fromList [MIND,BLUE,RED,GREAT,LONG,OLD,DRUNK,PLANT,POT,TEA,CABINET]

directions :: HashSet Lexeme
directions = HS.fromList [NORTH,EAST,SOUTH,WEST]

numbers :: HashSet Lexeme
numbers      = HS.fromList [ONE,TWO,THREE]

actors :: HashSet Lexeme
actors       = HS.fromList [FORD,WILLIAM]

symbol :: String -> Parser String
symbol = L.symbol sc

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"
    
data Lexeme
  = OPEN
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
  | TAKE
  | SINK
  | THROUGH
  | SEPERATOR
  deriving stock (Show,Eq,Enum, Ord)

instance ToText Lexeme where
  toText txt = toText (show txt :: String)

instance Hashable Lexeme where
   hashWithSalt = hashUsing fromEnum

term :: Parser Lexeme
term =
  PALACE           <$ symbol "PALACE"
  <|> BASEMENT     <$ symbol "BASEMENT"
  <|> ATTIC        <$ symbol "ATTIC"
  <|> MAZE         <$ symbol "MAZE"
  <|> DEN          <$ symbol "DEN"
  <|> PARLOUR      <$ symbol "PARLOUR"
  <|> VOIDLESSVOID <$ symbol "VOIDLESSVOID"
  <|> PLANT        <$ symbol "PLANT"
  <|> POT          <$ symbol "POT"
  <|> BOOK         <$ symbol "BOOK"
  <|> BELL         <$ symbol "BELL"
  <|> CANDLE       <$ symbol "CANDLE"
  <|> TEA          <$ symbol "TEA"
  <|> LOOK         <$ symbol "LOOK"
  <|> MOVE         <$ symbol "MOVE"
  <|> PUT          <$ symbol "PUT"
  <|> PLACE        <$ symbol "PLACE"
  <|> THAT         <$ symbol "THAT"
  <|> THIS         <$ symbol "THIS"
  <|> THE          <$ symbol "THE"
  <|> AT           <$ symbol "AT"
  <|> A            <$ symbol "A"
  <|> MY           <$ symbol "MY"
  <|> ME           <$ symbol "ME"
  <|> TO           <$ symbol "TO"
  <|> WITH         <$ symbol "WITH"
  <|> IN           <$ symbol "IN"
  <|> WHEN         <$ symbol "WHEN"
  <|> UNDER        <$ symbol "UNDER"
  <|> OVER         <$ symbol "OVER"
  <|> ABOVE        <$ symbol "ABOVE"
  <|> ON           <$ symbol "ON"
  <|> MIND         <$ symbol "MIND"
  <|> BLUE         <$ symbol "BLUE"
  <|> RED          <$ symbol "RED"
  <|> GREAT        <$ symbol "GREAT"
  <|> LONG         <$ symbol "LONG"
  <|> OLD          <$ symbol "OLD"
  <|> DRUNK        <$ symbol "DRUNK"
  <|> FORD         <$ symbol "FORD"
  <|> WILLIAM      <$ symbol "WILLIAM"
  <|> ONE          <$ symbol "ONE"
  <|> TWO          <$ symbol "TWO"
  <|> THREE        <$ symbol "THREE"
  <|> ALL          <$ symbol "ALL"
  <|> NORTH        <$ symbol "NORTH"
  <|> EAST         <$ symbol "EAST"
  <|> SOUTH        <$ symbol "SOUTH"
  <|> WEST         <$ symbol "WEST"
  <|> EXAMINE      <$ symbol "EXAMINE"
  <|> TAKE         <$ symbol "TAKE"
  <|> OPEN         <$ symbol "OPEN"
  <|> CABINET      <$ symbol "CABINET"
  <|> DOOR         <$ symbol "DOOR"
  <|> SINK         <$ symbol "SINK"
  <|> KITCHEN      <$ symbol "KITCHEN"
  <|> THROUGH      <$ symbol "THROUGH"
  <|> SEPERATOR    <$ symbol ","