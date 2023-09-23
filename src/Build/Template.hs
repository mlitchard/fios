module Build.Template where

import Language.Haskell.TH
    ( mkName,
      Exp(LitE, UnboundVarE, AppE, ConE),
      Pat(VarP),
      Type(ConT, AppT),
      Dec(ValD, SigD),
      DecsQ,
      Body(NormalB),
      Lit(IntegerL) )

import Tokenizer.Data ( Lexeme )


labelTemplate :: String -> String -> Lexeme -> DecsQ
labelTemplate typeStr binding lexeme = pure desc
  where
    desc        = [sigd,vald]
    sigd        = SigD decLabel appt
    appt        = AppT labelType' objectType'
    decLabel    = mkName (binding <> "Label")
    labelType'  = ConT labelName
    objectType' = ConT (mkName typeStr)
    vald = ValD (VarP decLabel) eval' []
    eval' = NormalB (AppE (ConE labelName) uvar)
    uvar = UnboundVarE $ (mkName . toString) lexeme
    labelName = mkName "Game.Model.Mapping.Label"

gidDeclaration :: String -> String -> Integer -> DecsQ
gidDeclaration tag' binding' literal = pure [sigd, value]
  where
    sigd    = SigD binding type'
    binding = mkName (binding' <> "GID")
    type'   = AppT (ConT gid) (ConT tag)
    gid     = mkName "GID"
    tag     = mkName tag'
    value = ValD (VarP binding) (NormalB (AppE (ConE gid) (LitE (IntegerL literal)))) []
