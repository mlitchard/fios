module Spike.RestrictedPair where

type Not :: Bool -> Bool
type family Not a where
  Not True = False
  Not False = True

{-
type MyPair :: Bool -> (Bool,Bool)
type family MyPair a b where
  MyPair False = (False,True)
  MyPair True  = (True,False)
-}

