module HauntedHouse.Game.Build.Template where

import Language.Haskell.TH

import HauntedHouse.Tokenizer (Lexeme)
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.World 

-- newtype Label a = Label {_unLabel :: Lexeme} deriving (Show,Eq,Ord)
objectProtoType :: DecsQ 
objectProtoType = [d| sinkLabel :: Label Object
                      sinkLabel = (Label SINK)
                  |]
labelTemplate :: String -> String -> Lexeme -> DecsQ 
labelTemplate typeStr name lexeme = pure desc
  where
    desc        = [sigd,vald]
    sigd        = SigD decLabel appt
    appt        = AppT labelType' objectType'
    decLabel    = mkName (name <> "Label")
    labelType'  = ConT labelName
    objectType' = ConT (mkName typeStr)
    vald = ValD (VarP decLabel) eval' []
    eval' = NormalB (AppE (ConE labelName) uvar)
    uvar = UnboundVarE $ (mkName . toString) lexeme
    labelName = mkName "HauntedHouse.Game.Model.Mapping.Label"

-- assumes unique int 

objectGIDDeclaration :: String -> Integer -> DecsQ 
objectGIDDeclaration dnameSTR gid = pure [fsig] 
  where
    dname = dnameSTR <> "GID"
    fsig = ValD fname objectType []
    fname = VarP (mkName dname)
    objectType = NormalB (AppE constructor value)
    constructor = ConE (mkName "GID")
    value = LitE (IntegerL gid)

locationGIDDeclaration :: String -> Integer -> DecsQ 
locationGIDDeclaration dnameSTR gid = pure [fsig]
  where
    fsig  = ValD fname objectType []
    fname = VarP (mkName dname) 
    dname =  dnameSTR <> "GID"
    objectType = NormalB (AppE constructor value)
    constructor = ConE (mkName "GID")
    value = LitE (IntegerL gid)
