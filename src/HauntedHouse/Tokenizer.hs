module HauntedHouse.Tokenizer 
    ( HauntedHouse.Tokenizer.Data.actors
    , HauntedHouse.Tokenizer.Data.adjectives
    , HauntedHouse.Tokenizer.Data.determiners 
    , HauntedHouse.Tokenizer.Data.directions
    , HauntedHouse.Tokenizer.Data.locations
    , HauntedHouse.Tokenizer.Data.nouns 
    , HauntedHouse.Tokenizer.Data.numbers 
    , HauntedHouse.Tokenizer.Data.objects
    , HauntedHouse.Tokenizer.Data.prepositions
    , HauntedHouse.Tokenizer.Data.term
    , HauntedHouse.Tokenizer.Data.verbs
    , HauntedHouse.Tokenizer.Data.Lexeme
    , runParser
    , HauntedHouse.Tokenizer.tokens ) where

import Prelude hiding (many) 
import HauntedHouse.Tokenizer.Data 
import Text.Megaparsec hiding (runParser)

import Data.Char (toUpper)

runParser :: Parser a -> String -> Either String a
runParser parser str = do
  case parse parser "" str' of
    Left err -> Left $ errorBundlePretty err
    Right prog -> Right prog
  where
    str' = map toUpper str

tokens :: Parser [Lexeme]
tokens = sc *> many term <* eof