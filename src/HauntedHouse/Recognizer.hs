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

  nounPhrase <-
    rule $
      Noun
        <$> noun
        <|> NounPhrase2
        <$> determiner
        <*> adjPhrase
        <*> nounPhrase
        <|> NounPhrase1
        <$> determiner
        <*> nounPhrase
        <|> NounPhrase3
        <$> number
        <*> nounPhrase
        <|> NounPhrase4
        <$> nounPhrase
        <*> prepPhrase
        <|> NounPhrase5
        <$> adjPhrase
        <*> nounPhrase
        <?> toText ("noun phrase" :: String)

  verbPhrase <-
    rule $
      OnlyVerb
        <$> verb
        <|> VerbPhrase1
        <$> verb
        <*> nounPhrase
        <|> VerbPhrase2
        <$> verb
        <*> prepPhrase
        <?> toText ("verb phrase" :: String)

  prepPhrase <-
    rule $
      PrepPhrase
        <$> preposition
        <*> nounPhrase
        <|> Preposition
        <$> preposition
        <?> toText ("prep phrase" :: String)

  adjPhrase <-
    rule $
      Adjective
        <$> adjective
        <?> toText ("adjective" :: String)

  return $ ImperativeClause <$> verbPhrase
