module HauntedHouse.Game.World.Template where

import HauntedHouse.Game.Labels ( ObjectLabel(ObjectLabel) )
import Language.Haskell.TH
import Data.Foldable
import GHC.Num qualified
import HauntedHouse.Tokenizer (Lexeme (..))
import HauntedHouse.Game.GID (GID (..))
import HauntedHouse.Game.Object (Object (..))

-- assumes unique int 
objectGIDDeclaration :: Integer -> String -> DecsQ 
objectGIDDeclaration gid dnameSTR = pure [fDec] 
  where
    dname = dnameSTR <> "GID"
    fDec = fSig
    fSig = ValD fname objectType []
    fname = VarP (mkName dname)
    objectType = NormalB (AppE constructor value)
    constructor = ConE (mkName "GID")
    value = LitE (IntegerL gid)


objectLabelDeclaration :: String -> Lexeme -> Q [Dec]
objectLabelDeclaration dnameSTR object = pure fDec
  where
    dname = dnameSTR <> "Label"
    fDec = [fSig, value]
    fSig = SigD fname objectType
    vPattern = VarP (mkName dname)
    value = ValD vPattern body []
    constructor = ConE (mkName "ObjectLabel")
    body = NormalB (AppE constructor lexeme)
    lexeme = UnboundVarE (mkName (show object))
    objectType = ConT (mkName "ObjectLabel")
    fname = mkName dname
