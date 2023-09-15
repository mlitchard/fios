module HauntedHouse.Recognizer (
  module Text.Earley

, module HauntedHouse.Recognizer
, module HauntedHouse.Recognizer.WordClasses
) where

import Data.HashSet qualified as HS

import HauntedHouse.Tokenizer
import Text.Earley

import HauntedHouse.Recognizer.WordClasses

imperative :: Grammar r (Prod r Text Lexeme Imperative)
imperative = mdo
  noun <-
    rule $
      satisfy (`HS.member` nouns)
        <?> toText ("noun" :: String)

  verb <-
    rule $
      satisfy (`HS.member` verbs)
        <?> toText ("verb" :: String)

  determiner <-
    rule $
      satisfy (`HS.member` determiners)
        <?> toText ("determiner" :: String)

  adjective <-
    rule $
      satisfy (`HS.member` adjectives)
        <?> toText ("adjective" :: String)

  preposition <-
    rule $
      satisfy (`HS.member` prepositions)
        <?> toText ("preposition" :: String)

  number <-
    rule $
      satisfy (`HS.member` numbers)
        <?> toText ("number" :: String)

-- NounPhrase2 Determiner AdjPhrase
  nounPhrase <-
    rule $
      Noun <$> noun
        <|> NounPhrase1 <$> determiner <*> nounPhrase
        <|> NounPhrase2 <$> adjPhrase <*> nounPhrase 
        <|> NounPhrase3 <$> number <*> nounPhrase
        <?> toText ("noun phrase" :: String)
{-
VerbPhrase7 Verb NounPhrase PrepPhrase
-}

  verbPhrase <-
    rule $
      OnlyVerb <$> verb 
        <|> VerbPhrase4 <$> verb <*> prepPhrase <*> prepPhrase <*> prepPhrase
        <|> VerbPhrase1 <$> verb <*> nounPhrase
        <|> VerbPhrase2 <$> verb <*> prepPhrase
        <|> VerbPhrase3 <$> verb <*> prepPhrase <*> prepPhrase 
        <|> VerbPhrase5 <$> verb <*> adjPhrase 
        <|> VerbPhrase6 <$> verb <*> adjPhrase <*> prepPhrase 
        <|> VerbPhrase7 <$> verb <*> nounPhrase <*> prepPhrase 
        <?> toText ("verb phrase" :: String)

-- | PrepPhrase2 Preposition Determiner Adjective NounPhrase
  prepPhrase <-
    rule $
      PrepPhrase1 <$> preposition <*> nounPhrase
        <|> PrepPhrase2 <$> preposition <*> determiner <*> adjPhrase <*> nounPhrase
       -- <|> Preposition <$> preposition
        <?> toText ("prep phrase" :: String)
-- AdjNoun Adjective NounPhrase
  adjPhrase <-
    rule $
      Adjective <$> adjective
        <|> AdjNoun <$> determiner <*> adjPhrase <*> nounPhrase 
        <|> AdjPrep <$> determiner <*> adjPhrase <*> prepPhrase
        <?> toText ("adjective" :: String)
        
{-
data Imperative
  = ImperativeClause VerbPhrase
  | ClarifyingClause1 PrepPhrase 
  | ClarifyingClause2 PrepPhrase PrepPhrase
  | ClarifyingClause3 PrepPhrase PrepPhrase
  | ClarifyingClause4 AdjPhrase
  | ClarifyingClause5 AdjPhrase PrepPhrase
  | ClarifyingClause6 AdjPhrase PrepPhrase 
-}
  return $ ImperativeClause <$> verbPhrase
            <|> ClarifyingClause1 <$> nounPhrase <*> prepPhrase 
            <|> ClarifyingClause2 <$> nounPhrase <*> prepPhrase <*> prepPhrase 
            <|> ClarifyingClause3 
                  <$> nounPhrase <*> prepPhrase <*> prepPhrase <*> prepPhrase
            <|> ClarifyingClause4 <$> adjPhrase 
            <|> ClarifyingClause5 <$> adjPhrase <*> prepPhrase 
            <|> ClarifyingClause6 <$> adjPhrase <*> prepPhrase <*> prepPhrase 
