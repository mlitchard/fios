{-# LANGUAGE AllowAmbiguousTypes #-} 
{-# LANGUAGE ConstraintKinds #-} 
{-# LANGUAGE DataKinds #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use 'natVal' from Relude" #-}
{-# HLINT ignore "Use 'Proxy' from Relude" #-}

module Types.ChapterEleven where 

import Data.Kind (Type)
import Data.Proxy
import Fcf
import GHC.TypeLits hiding (type (+)) 
import Unsafe.Coerce
import Prelude hiding (natVal)

data OpenSum (f :: k -> Type) (ts :: [k]) where
  UnsafeOpenSum :: Int -> f t -> OpenSum f ts 

type FindElem (key :: k) (ts :: [k]) = 
  FromMaybe Stuck =<< FindIndex (TyEq key) ts

type Member t ts = KnownNat (Eval (FindElem t ts))

findElem :: forall t ts. Member t ts => Int 
findElem = fromIntegral . natVal $ Proxy @(Eval (FindElem t ts))

inj :: forall f t ts. Member t ts => f t -> OpenSum f ts 
inj = UnsafeOpenSum (findElem @t @ts)

prj :: forall f t ts. Member t ts => OpenSum f ts -> Maybe(f t)
prj (UnsafeOpenSum i f) = 
  if i == findElem @t @ts
    then Just $ unsafeCoerce f
    else Nothing 


decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 t) = Left $ unsafeCoerce t 
decompose (UnsafeOpenSum n t) = Right $ UnsafeOpenSum (n - 1) t

weaken :: OpenSum f ts -> OpenSum f (x ': ts)
weaken (UnsafeOpenSum i ft) = UnsafeOpenSum newI ft
  where 
    newI = i + 1 

{-
let i = findElem @key @ts
      (xs,xs') = V.splitAt i v 
      removed = V.init xs 
  in OpenProduct $ removed V.++ xs'
  -}