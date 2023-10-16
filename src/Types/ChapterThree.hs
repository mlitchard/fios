module Types.ChapterThree where 

newtype T1 a = T1 (Int -> a)

instance Functor T1 where 
  fmap f (T1 g) = T1 (f . g) 

newtype T5 a = T5 ((a -> Int) -> Int)


