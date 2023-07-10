module HauntedHouse.Parser.WordClasses where

import HauntedHouse.Tokenizer (Lexeme)

type Noun = Lexeme
type Verb = Lexeme
type Determiner = Lexeme
type Preposition = Lexeme
type Adjective = Lexeme
type Number = Lexeme

newtype AdjPhrase = Adjective Adjective deriving stock (Show, Eq, Ord)

data NounPhrase
  = NounPhrase1 Determiner NounPhrase
  | NounPhrase2 Determiner AdjPhrase NounPhrase
  | NounPhrase3 Number NounPhrase
  | NounPhrase4 NounPhrase PrepPhrase
  | NounPhrase5 AdjPhrase NounPhrase
  | Noun Noun
  deriving stock (Show, Eq, Ord)

data VerbPhrase
  = VerbPhrase1 Verb NounPhrase
  | VerbPhrase2 Verb PrepPhrase
  | OnlyVerb Verb
  deriving stock (Show, Eq, Ord)

data PrepPhrase
  = PrepPhrase Preposition NounPhrase
  | Preposition Preposition
  deriving stock (Show, Eq, Ord)

newtype Imperative
  = ImperativeClause VerbPhrase
  deriving stock (Show, Eq, Ord)
