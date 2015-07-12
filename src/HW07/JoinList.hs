module HW07.JoinList where

import HW07.Sized
--import HW07.Scrabble
--import HW07.Buffer

import Data.Monoid

newtype And = And Bool
  deriving (Eq, Ord, Show)

getAnd :: And -> Bool
getAnd (And a) = a

newtype Or = Or Bool
  deriving (Eq, Ord, Show)

getOr :: Or -> Bool
getOr (Or a) = a

instance Monoid And where
  mempty = And True
  And a `mappend` And b = And $ a && b

instance Monoid Or where
  mempty = Or False
  Or a `mappend` Or b = Or $ a || b

{-
data JoinListBasic a = Empty
                     | Single a
                     | Append (JoinListBasic a) (JoinListBasic a)

jlbToList :: JoinListBasic a -> [a]
jlbToList Empty = []
jlbToList (Single a) = [a]
jlbToList (Append l1 l2) = jlbToList l1 ++ jlbToList l2
-}

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

-- ex1

-- Append op for Joinlist - yields a new JoinList whose monoidal
-- annotation is derived from those of the two arguments.
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
n1 +++ n2 = Append (tag n1 <> tag n2) n1 n2

-- Gets the annotation at the root of a JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- ex2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i Empty = Nothing
indexJ i jl@(Single m a) | (getSize . size) m == i = Just a
                         | otherwise               = Nothing
indexJ i (Append _ jl1 jl2) | (getSize . size) (tag jl1) > i = indexJ i jl1
                            | otherwise                      = indexJ i jl1

(!!?) :: [a] -> Int -> Maybe a
[] !!? _        = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0    = Just x
(x:xs) !!? i    = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
