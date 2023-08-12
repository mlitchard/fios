module HauntedHouse.Build.Descriptives where

import HauntedHouse.Tokenizer (adjectives, Lexeme)
import qualified Data.HashSet (toList)
import qualified Data.Char (toLower)

descriptives' :: [Lexeme]
descriptives' = Data.HashSet.toList adjectives

descriptivesLabelValuePair :: [(String,Lexeme)]
descriptivesLabelValuePair = zip descriptivesNames descriptives'

descriptivesName :: Lexeme -> String
descriptivesName l = map Data.Char.toLower $ toString l

descriptivesNames :: [String]
descriptivesNames = map descriptivesName descriptives'

