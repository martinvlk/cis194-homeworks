{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Applicative ((<$>), (<*>), pure)

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import HW07.JoinList hiding (main)
import HW07.Sized
import HW07.Scrabble
import HW07.Buffer

main :: IO ()
main = TF.defaultMain tests

tests :: [TF.Test]
tests = [ testGroup "QuickCheck HW07" [
              testProperty "indexJ" prop_indexJ,
              testProperty "dropJ" prop_dropJ,
              testProperty "takeJ" prop_takeJ
--              testProperty "toFromString" prop_toFromString
              ],
          testGroup "HUnit HW07" [
{-            let i = 3
                jl = (Append (Size 0) (Single (Size 1) 1) (Append (Size 1) (Append (Size 1) Empty (Single (Size 2) 2)) (Single (Size 2) 5)))::JoinList Size Int in
            testCase "indexJ" (indexJ i jl @=? jlToList jl !!? i)-}
            ]
        ]

instance Arbitrary (JoinList Size Int) where
  arbitrary = oneof [pure Empty,
                     Single (Size 1) <$> arbitrary,
                     (+++) <$> arbitrary <*> arbitrary]

prop_indexJ :: Int -> (JoinList Size Int) -> Bool
prop_indexJ i jl = (indexJ i jl) == (jlToList jl !!? i)

prop_dropJ :: Int -> (JoinList Size Int) -> Bool
prop_dropJ n jl = jlToList (dropJ n jl) == drop n (jlToList jl)

prop_takeJ :: Int -> (JoinList Size Int) -> Bool
prop_takeJ n jl = jlToList (takeJ n jl) == take n (jlToList jl)

prop_toFromString s = toString (fromString s:: (JoinList (Score, Size) String))
                                  == {-stripLastLineEnd -}s
--  where stripLastLineEnd = reverse . bool id tail . (\s->if head s=='\n') . reverse
