{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
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

module Types.ChapterElevenThree where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import Fcf hiding (Any)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (natVal, Any)

data Any (f :: k -> Type) where
  Any :: f t -> Any f

data OpenProduct (f :: k -> Type) (ts :: [(Symbol, k)]) where
  OpenProduct :: V.Vector (Any f) -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty

data Key (key :: Symbol) = Key

type UniqueKey (key :: k) (ts :: [(k, t)]) =
  Null =<< Filter (TyEq key <=< Fst) ts

insert :: Eval (UniqueKey key ts) ~ 'True => Key key
                                              -> f t
                                              -> OpenProduct f ts
                                              -> OpenProduct f ('(key, t) ': ts)
insert _ ft (OpenProduct v) = OpenProduct $ V.cons (Any ft) v

type FindElem (key :: Symbol) (ts :: [(Symbol, k)]) =
  Eval (FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts)

findElem :: forall key ts . KnownNat (FindElem key ts) => Int
findElem = fromIntegral . natVal $ Proxy @(FindElem key ts)

type LookupType (key :: k) (ts :: [(k, t)]) =
  FromMaybe Stuck =<< Lookup key ts

get' :: forall key ts f. KnownNat (FindElem key ts) => Key key
                                                        -> OpenProduct f ts
                                                        -> f (Eval(LookupType key ts))
get' _ (OpenProduct v) =
  let x = findElem @key @ts
  in unAny $ V.unsafeIndex v x 
  where
    unAny(Any a) = unsafeCoerce a

type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) =
  SetIndex (FindElem key ts) '(key, t) ts

update :: forall key ts t f. KnownNat (FindElem key ts) => Key key
                                                            -> f t
                                                            -> OpenProduct f ts
                                                            -> OpenProduct f (Eval (UpdateElem key t ts))
update _ ft (OpenProduct v) = 
  let find' = [(findElem @key @ts, Any ft)]
  in OpenProduct $ v V.// find'

{-
data Filter :: (a -> Exp Bool) -> [a] -> Exp [a]Source#

Keep all elements that satisfy a predicate, remove all that don't.

Example
>>> :kind! Eval (Filter ((>) 3) '[1,2,3,0])
Eval (Filter ((>) 3) '[1,2,3,0]) :: [Nat]
= '[1, 2, 0]
-}
-- delete :: Key key -> OpenProduct f ts 
type DeleteElem (key :: Symbol) (ts :: [(Symbol, k)]) =
  Filter (Not <=< TyEq key) ts
{-
delete :: forall key ts f. KnownSymbol @key => Key key 
            -> OpenProduct f ts 
            -> OpenProduct f (Eval (DeleteElem key ts))
delete _ (OpenProduct v) = OpenProduct 
-}

delete :: forall key ts ts' f. (KnownNat (FindElem key ts)) => Key key
                                                                -> OpenProduct f ts
                                                                -> OpenProduct f ts' -- f t -> OpenProduct f ts -> OpenProduct f ts
delete _ (OpenProduct v) =
  let i = findElem @key @ts
      (xs',xs) = V.splitAt i v 
      removed = V.init xs' 
  in OpenProduct $ removed V.++ xs  
