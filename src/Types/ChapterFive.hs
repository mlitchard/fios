{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

{-
:m + Relude TopLevel Build.Default Tokenizer Recognizer Build.ObjectTemplate
(runStateT . runExceptT) (runReaderT start config) defaultGameState
-}
module Types.ChapterFive where

import Data.Kind (Constraint, Type)
import Prelude hiding (show, All)
import GHC.Show (Show(..))


data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t-> HList ts -> HList(t ': ts)
infixr 5 :#

{-
instance Eq (HList '[]) where
  (==) :: HList ('[] @Type) -> HList ('[] @Type) -> Bool
  HNil == HNil = True
  -}
-- (a:#as)==(b:#bs)=a==b&&as==bs
{-
instance (Eq t , Eq (HList ts)) => Eq (HList (t ': ts)) where
  (a :# as) == (b :# bs) = (a == b) && as == bs
-}

{-
instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) 􏰀→ where
(a:#as)==(b:#bs)=a==b&&as==bs
-}
{-
instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
  (a :# as) == (b :# bs) = a == b && as == bs

instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
  compare p q
    | p > q = GT
    | p < q = LT
    | otherwise = EQ

instance (Show t, Show (HList ts)) => Show (HList (t ': ts )) where 
  show (x :# xs) = show x <> show xs
-}

-- Don't use relude for these exercises

type family All (c :: Type -> Constraint)
                (ts :: [Type]) :: Constraint where
  All c '[]       = ()  -- ! 1
  All c (t ': ts) = (c t, All c ts)  -- ! 2

type family AllEq (ts :: [Type]) :: Constraint where
  AllEq '[]       = ()  -- ! 1
  AllEq (t ': ts) = (Eq t, AllEq ts)  -- ! 2

instance All Eq ts => Eq (HList ts) where 
  HNil == HNil = True 
  (a :# as) == (b:#bs) = a == b && as == bs 

instance (All Ord ts, All Eq ts) => Ord (HList ts) where 
  compare p q 
    | p < q = LT 
    | p > q = GT 
    | otherwise = EQ 


instance (All Show ts) => Show (HList ts) where 
  show HNil = show ("HNIL" :: String)
  show (x :# xs) = show x <> show xs 


newtype Cont a = Cont {unCont :: forall r. (a -> r) -> r }

{-
instance Functor Cont where 
  fmap f (Cont c) = Cont $\c'-> c (c' . f)
-}
instance Functor Cont where 
  fmap f (Cont c) = Cont $ \c' -> c (c' . f)

instance Applicative Cont where 
  (Cont f) <*> (Cont x) = Cont $ \f' -> f (f' . x)

  pure x = Cont $ \c -> c x

-- instance Monad Cont where 
--  return = pure 
 -- (Cont c) >>= f = (\c' -> )