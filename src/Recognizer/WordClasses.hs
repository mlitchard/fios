module Recognizer.WordClasses where

import Tokenizer (Lexeme)

type Noun = Lexeme
type Verb = Lexeme
type Determiner = Lexeme
type Preposition = Lexeme
type Adjective = Lexeme
type Number = Lexeme
-- (runStateT . runExceptT) (runReaderT start config) defaultGameState
data AdjPhrase 
      = Adjective Adjective 
      | AdjPrep Determiner AdjPhrase PrepPhrase
      | AdjNoun Determiner AdjPhrase NounPhrase 
          deriving stock (Show, Eq, Ord)

data NounPhrase
  = NounPhrase1 Determiner NounPhrase
  | NounPhrase2 Adjective NounPhrase
  | NounPhrase3 Number NounPhrase
  | Noun Noun
  deriving stock (Show, Eq, Ord)

data VerbPhrase
  = VerbPhrase1 Verb NounPhrase
  | VerbPhrase2 Verb PrepPhrase
  | VerbPhrase3 Verb PrepPhrase PrepPhrase 
  | VerbPhrase4 Verb PrepPhrase PrepPhrase PrepPhrase 
  | VerbPhrase5 Verb AdjPhrase
  | VerbPhrase6 Verb AdjPhrase PrepPhrase
  | VerbPhrase7 Verb NounPhrase PrepPhrase
  | OnlyVerb Verb
  deriving stock (Show, Eq, Ord)

data PrepPhrase
  = PrepPhrase1 Preposition NounPhrase
  | PrepPhrase2 Preposition  Determiner Adjective NounPhrase
  -- | Preposition Preposition
  deriving stock (Show, Eq, Ord)

data Imperative
  = ImperativeClause VerbPhrase
  | ClarifyingClause1 NounPhrase PrepPhrase 
  | ClarifyingClause2 NounPhrase PrepPhrase PrepPhrase
  | ClarifyingClause3 NounPhrase PrepPhrase PrepPhrase PrepPhrase
  | ClarifyingClause4 AdjPhrase
  | ClarifyingClause5 AdjPhrase PrepPhrase
  | ClarifyingClause6 AdjPhrase PrepPhrase  PrepPhrase 
 
    deriving stock (Show, Eq, Ord)
