module HW04.HW4 where

import Data.Monoid (Product(..), getProduct)
import Data.List (sort, (\\))
import Data.Foldable (foldMap)
--import Debug.Trace (trace)

-- ex11
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs) | even x    = (x - 2) * fun1 xs
            | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (+ (-2)) . filter even

fun1'' :: [Integer] -> Integer
fun1'' = getProduct . foldMap (Product . (+ (-2))) . filter even

-- ex12
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3*n + 1)

-- https://en.wikipedia.org/wiki/Collatz_conjecture
fun2' :: Integer -> Integer
fun2'  = sum . filter even . takeWhile (>1) . iterate (\n->if even n
                                                           then n `div` 2
                                                           else 3*n + 1)

-- ex2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

depth :: Tree a -> Integer
depth Leaf = -1
depth (Node d _ _ _) = d

instance (Show a, Eq a) => Ord (Tree a) where
  left `compare` right = depth left `compare` depth right

foldTree :: (Show a, Eq a) => [a] -> Tree a
foldTree = foldr insert Leaf

insert :: (Show a, Eq a) => a -> Tree a -> Tree a
insert v Leaf = Node 0 Leaf v Leaf
insert newv (Node _ left v right) = Node newd left' v right'
  where [ins,oth] = sort [left, right]
        newt = insert newv ins
        (left', right') | oth==right = (newt, right)
                        | otherwise  = (left, newt)
        newd = succ . depth $ max newt oth

-- ex31
xor :: [Bool] -> Bool
--xor = odd . length . filter (==True)
--xor = foldr (/=) False
xor = foldr xorit False
  where xorit True  acc = not acc
        xorit False acc = acc

-- ex32
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []
-- map' f = foldr ((:) . f) [] -- better!

-- ex3o

-- First implement both folds recursively to get clarity on how they work

-- foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
myFoldr :: (b -> a -> a) -> a -> [b] -> a
myFoldr _ base [] = base
myFoldr f base (x:xs) = f x (myFoldr f base xs)

-- foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn
myFoldl' :: (a -> b -> a) -> a -> [b] -> a
myFoldl' _ base [] = base
myFoldl' f base (x:xs) = myFoldl' f (f base x) xs

-- now the exercise
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs =
   foldr (\x acf acc' -> acf (f acc' x)) id xs base
--myFoldl f = flip $ foldr (\x acf -> acf . (`f` x)) id

-- ex4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1).(*2)) . (\\) [1..n]
                  $ [x | j<-[1..n], i<-[1..j], let x = i+j+2*i*j, x<=n]
