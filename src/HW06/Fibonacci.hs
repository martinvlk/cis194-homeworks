{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module HW06.Fibonacci where

-- ex1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

-- ex2
fibs2 :: [Integer]
fibs2 = map fst . iterate (\(p1, p2) -> (p2, p1+p2)) $ (0, 1)

-- ex3
data Stream a = a ::: Stream a
infixr 5 :::

streamToList :: Stream a -> [a]
streamToList ~(x:::s) = x : streamToList s

instance Show a => Show (Stream a) where
  show = ("Stream " ++) . (++ "......ad infinitum.") . show . take 100 . streamToList

instance Functor Stream where
  fmap f ~(e:::s) = f e ::: fmap f s

{- Monoid on an infinite structure doesn't make sense.

instance Monoid Stream where
  mempty = 
  mappend =
-}

instance Foldable Stream where
  {- there is no base case - streams are infinite
   - ...well it means Foldable instance doesn't make sense for Stream...
   - because it never produces the final answer
   -}
  foldr f base ~(x:::s) = f x (foldr f base s)

-- ex4
streamRepeat :: a -> Stream a
streamRepeat = streamFromSeed id

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = fmap

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = x ::: streamFromSeed f (f x)

-- ex5
nats :: Stream Integer
nats = streamFromSeed succ 0

{- 1 interleaved with (2 interleaved with (3 interleaved with ( ... )))
 - Source: https://oeis.org/A001511
 -}
ruler :: Stream Integer
ruler = go 0
  where go n = streamRepeat n `interleaveStreams` go (succ n)

{- Source: https://wiki.haskell.org/Maintaining_laziness#Strict_pattern_matching_in_a_recursion
 -}
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams ~(x1:::s1) ~(x2:::s2) = x1 ::: x2 ::: interleaveStreams s1 s2

streamDrop :: Int -> Stream a -> Stream a
streamDrop 0 s = s
streamDrop n ~(_:::s) = streamDrop (pred n) s

-- ex6
streamZipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamZipWith f ~(x1:::s1) ~(x2:::s2) = f x1 x2 ::: streamZipWith f s1 s2

instance Num (Stream Integer) where
  fromInteger n = n ::: streamRepeat 0
  negate = fmap negate
  (+) = streamZipWith (+)
  ~(a:::as) * ~b'@(b:::bs) = a*b ::: fmap (*a) bs + as * b'

instance Fractional (Stream Integer) where
  ~(a:::as) / ~(b:::bs) = q
    where q = a `div` b ::: fmap (`div` b) (as-q*bs)
    
fibs3 :: Stream Integer
fibs3 = let x' = 0 ::: 1 ::: streamRepeat 0 in
  x' / (1 - x' - x'^2)

-- ex7

data Matrix = Mx Integer Integer
                 Integer Integer
            deriving Show

instance Num Matrix where
  (Mx a11 a12
      a21 a22) * (Mx b11 b12
                     b21 b22) =
    Mx (a11 * b11 + a12 * b21) (a11 * b12 + a12 * b22)
       (a21 * b11 + a22 * b21) (a21 * b12 + a22 * b22)

fibs4 :: [Integer]
fibs4 = 0 : 1 : [fi | n<-[1..], let (Mx fi _ _ _) = m^n]
  where m = Mx 1 1 1 0

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 1
fib4 n = fi
  where m = Mx 1 1 1 0
        (Mx fi _ _ _) = m^(n-2)
