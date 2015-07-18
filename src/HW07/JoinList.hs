{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HW07.JoinList where

import Data.Monoid

import HW07.Sized
import HW07.Scrabble
import HW07.Buffer
import HW07.Editor

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
(!!?) :: [a] -> Int -> Maybe a
[] !!? _        = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0    = Just x
(_:xs) !!? i    = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty      = Nothing
indexJ i _  | i < 0 = Nothing
indexJ i (Single s a)
  | (getSize . size $ s) == i+1 = Just a
  | otherwise                   = Nothing
indexJ i (Append s jl1 jl2)
  | sze s < i+1   = Nothing
  | jl1sze >= i+1 = indexJ i jl1
  | otherwise     = indexJ (i-jl1sze) jl2
  where jl1sze = sze $ tag jl1
        sze    = getSize . size

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl | n < 0 = jl
dropJ n jl@(Single s _)
  | (getSize . size $ s) > n = jl
  | otherwise                = Empty
dropJ n (Append s jl1 jl2)
  | sze s <= n  = Empty
  | jl1sze >= n = dropJ n jl1 +++ jl2
  | otherwise   = dropJ (n-jl1sze) jl2
  where jl1sze = sze $ tag jl1
        sze    = getSize . size

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n<=0 = Empty
takeJ _ Empty = Empty
takeJ _ jl@(Single _ _) = jl
takeJ n jl@(Append s jl1 jl2)
  | n >= sze s = jl
  | n < jl1sze = takeJ n jl1
  | otherwise  = jl1 +++ takeJ (n-jl1sze) jl2
  where jl1sze = sze $ tag jl1
        sze    = getSize . size

-- ex3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- ex4
type ScoreSizeList = JoinList (Score, Size) String

instance Buffer ScoreSizeList where
  toString Empty = mempty
  toString (Single _ d) = d
  toString (Append _ Empty d2) = toString d2
  toString (Append _ d1 Empty) = toString d1
  toString (Append _ d1 d2) = concat [toString d1, "\n", toString d2]

  -- use halving strategy so that we end up with a balanced tree
  fromString = list2Jl . lines
    where list2Jl [] = Empty
          list2Jl [l] = Single (scoreString l, Size 1) l
          list2Jl ls = list2Jl h1 +++ list2Jl h2
            where (h1, h2) = splitAt (length ls `div` 2) ls

  line n jl | n < 0 = Nothing
            | otherwise = case (takeJ 1 . dropJ n) $ jl of
              Empty -> Nothing
              jl'   -> Just $ toString jl'

  replaceLine n s jl = takeJ n jl +++ fromString s +++ dropJ (n+1) jl
  numLines jl = getSize . snd . tag $ jl
  value jl = getScore . fst . tag $ jl

main :: IO ()
main = runEditor editor (((fromString . unlines)
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ])::ScoreSizeList)
