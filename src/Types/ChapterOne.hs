module Types.ChapterOne where

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

import Data.Kind
import GHC.TypeLits

type ToTerms :: [Symbol] -> Constraint
class ToTerms a where
  asTerms :: Proxy a -> [String]

instance forall x xs. (KnownSymbol x, ToTerms xs) => ToTerms (x ': xs) where
  asTerms _ = symbolVal (Proxy @x) : asTerms (Proxy @xs)

instance ToTerms '[] where
  asTerms _ = []

type MyList = ["foo", "bar"]

--test =  putStrLn . show $ asTerms @MyList 
