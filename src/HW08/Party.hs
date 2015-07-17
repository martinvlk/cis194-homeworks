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
treeFold :: (Tree a -> [b] -> b) -> Tree a -> b
treeFold f n@(Node _ ns) = f n $ map (treeFold f) ns

-- ex3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gls = ((e `glCons`) . mconcat . map snd $ gls,
                                  mconcat . map moreFun $ gls)

-- ex4
maxFun :: Tree Employee -> GuestList
maxFun = moreFun . treeFold (nextLevel . rootLabel)

-- ex5
instance Show GuestList where
  show (GL es fun) = concat ["Total Fun: ", show fun, "\n",
                             concat . sort . map ((++"\n") . empName) $ es]

main :: IO ()
main = readFile "src/HW08/company.txt" >>= print . maxFun . read
