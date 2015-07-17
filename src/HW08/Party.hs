{-# OPTIONS_GHC -fno-warn-orphans #-}

module HW08.Party where

import Data.Monoid
import Data.Tree
import Data.List (sort)

import HW08.Employee

-- ex1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (empFun e + f)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es1 fun1)
          (GL es2 fun2) = GL (es1 <> es2) (fun1 + fun2)

moreFun :: (GuestList, GuestList) -> GuestList
moreFun = uncurry max

-- ex2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node v ns) = f v $ map (treeFold f) ns

-- alternative version of fold, not usable for this exercise,
-- but is closer to standard fold.
treeFold' :: (a -> b -> b) -> b -> Tree a -> b
treeFold' f base (Node v ns) = f v $ foldr (flip (treeFold' f)) base ns

-- ex3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gls = ((e `glCons`) . mconcat . map snd $ gls,
                                  mconcat . map moreFun $ gls)

-- ex4
maxFun :: Tree Employee -> GuestList
maxFun = moreFun . treeFold nextLevel

-- ex5
instance Show GuestList where
  show (GL es fun) = concat ["Total Fun: ", show fun, "\n",
                             concat . sort . map ((++"\n") . empName) $ es]

main :: IO ()
main = readFile "src/HW08/company.txt" >>= print . maxFun . read

{-
treeFold (\n acc->sum acc + (empFun. rootLabel $ n))
treeFold (\n acc->concat acc ++ (empName . rootLabel $ n))
-}
