{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Monoid
import Control.Applicative ((<$>), (<*>), pure)

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import HW07.JoinList
import HW07.Sized

main :: IO ()
main = TF.defaultMain tests

tests :: [TF.Test]
tests = [ testGroup "QuickCheck HW07" [
              testProperty "indexJ" prop_indexJ
              ],
          testGroup "HUnit HW07" [
            t1
            ]
        ]

t1 = testCase "indexJ" (indexJ i jl @=? jlToList jl !!? i)
  where i = 3
        jl = Append (Size 0) (Single (Size 1) 1) (Append (Size 1) (Append (Size 1) Empty (Single (Size 2) 2)) (Single (Size 2) 5))

prop_indexJ :: Int -> (JoinList Size Int) -> Bool
prop_indexJ i jl = (indexJ i jl) == (jlToList jl !!? i)

instance Arbitrary (JoinList Size Int) where
  arbitrary = oneof [pure Empty,
                     Single (Size 1) <$> arbitrary,
                     (+++) <$> arbitrary <*> arbitrary]

instance Arbitrary Size where
    arbitrary = Size <$> arbitrary
