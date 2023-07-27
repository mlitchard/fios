module HauntedHouse.Game.World.Template where

import Language.Haskell.TH
    ( mkName,
      Exp(LitE, ConE, AppE),
      Pat(VarP),
      Dec(ValD),
      DecsQ,
      Body(NormalB),
      Lit(IntegerL) )

-- assumes unique int 
objectGIDDeclaration :: Integer -> String -> DecsQ 
objectGIDDeclaration gid dnameSTR = pure [fsig] 
  where
    dname = dnameSTR <> "GID"
    fsig = ValD fname objectType []
    fname = VarP (mkName dname)
    objectType = NormalB (AppE constructor value)
    constructor = ConE (mkName "GID")
    value = LitE (IntegerL gid)

locationGIDDeclaration :: Integer -> String -> DecsQ 
locationGIDDeclaration gid dnameSTR = pure [fsig]
  where
    fsig  = ValD fname objectType []
    fname = VarP (mkName dname) 
    dname =  dnameSTR <> "GID"
    objectType = NormalB (AppE constructor value)
    constructor = ConE (mkName "GID")
    value = LitE (IntegerL gid)
