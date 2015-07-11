module HW07.JoinList where

import HW07.Sized
import HW07.Scrabble
import HW07.Buffer

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

data JoinListBasic a = Empty
                     | Single a
                     | Append (JoinListBasic a) (JoinListBasic a)

jlbToList :: JoinListBasic a -> [a]
jlbToList Empty = []
jlbToList (Single a) = [a]
jlbToList (Append l1 l2) = jlbToList l1 ++ jlbToList l2

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

-- ex1
