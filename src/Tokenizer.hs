module Tokenizer (
  Tokenizer.Data.actors,
  Tokenizer.Data.adjectives,
  Tokenizer.Data.determiners,
  Tokenizer.Data.directions,
  Tokenizer.Data.locations,
  Tokenizer.Data.nouns,
  Tokenizer.Data.numbers,
  Tokenizer.Data.objects,
  Tokenizer.Data.prepositions,
  Tokenizer.Data.term,
  Tokenizer.Data.verbs,
  Tokenizer.Data.Lexeme (..),
  runParser,
  Tokenizer.tokens,
) where

import Tokenizer.Data
import Text.Megaparsec hiding (runParser)
import Prelude hiding (many)

import Data.Char (toUpper)

runParser :: Parser a -> Text -> Either String a
runParser parser str = do
  case parse parser "" str' of
    Left err -> Left $ errorBundlePretty err
    Right prog -> Right prog
  where
    str' = map toUpper $ toString str

tokens :: Parser [Lexeme]
tokens = sc *> many term <* eof

