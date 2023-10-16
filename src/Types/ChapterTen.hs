{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Types.ChapterTen where

import Prelude hiding (Map, fst)
import Data.Kind (Constraint, Type)
{-
class Eval l t | l -> t where
  eval :: l -> t

newtype ListToMaybe a = ListToMaybe [a]

instance Eval (ListToMaybe a) (Maybe [a]) where
  eval (ListToMaybe []) = Nothing 
  eval (ListToMaybe (x : _) = Just x 
-}
type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data Snd :: (a,b) -> Exp b

type instance Eval (Snd '(a, b)) = b

data ListToMaybe :: [a] -> Exp (Maybe a)

type instance Eval (ListToMaybe (a ': _1)) = 'Just [a]
type instance Eval (ListToMaybe '[]) = 'Nothing

data MapList :: (a -> Exp b) -> [a] -> Exp [b]

type instance Eval (MapList f '[]) = '[]

type instance Eval (MapList f (a ': as)) = Eval (f a) ': Eval (MapList f as)
-- (a->b->b)->b->[a]->b
data FoldR :: (a -> b -> Exp b) -> b -> [a] -> Exp b

{-
data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type instance Eval (Foldr _1 b '[]) = b
type instance Eval (Foldr f b (a ': as)) =
Eval (f a (Eval (Foldr f b as)))
-}
type instance Eval (FoldR _ b '[]) = b
type instance Eval (FoldR f b (a ': as)) = (Eval ( f a (Eval (FoldR f b as))))

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b 
myFoldr f b' (x:xs) = myFoldr f (f x b') xs

{-
type instance Eval (Map f ('Left x)) = 'Left x
type instance Eval (Map f ('Right a)) = 'Right (Eval (f a))
-}
data Map :: (a -> Exp b) -> f a -> Exp (f b)

type instance Eval (Map f '(a,b)) = '(a, Eval (f b))