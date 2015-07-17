{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import HW08.Party

main :: IO ()
main = TF.defaultMain tests

tests :: [TF.Test]
tests = [ testGroup "QuickCheck HW08" [
--              testProperty "indexJ" prop_indexJ,
              ],
          testGroup "HUnit HW07" [
{-            let i = 3
                jl = (Append (Size 0) (Single (Size 1) 1) (Append (Size 1) (Append (Size 1) Empty (Single (Size 2) 2)) (Single (Size 2) 5)))::JoinList Size Int in
            testCase "indexJ" (indexJ i jl @=? jlToList jl !!? i)-}
            ]
        ]
