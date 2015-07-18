{-# LANGUAGE FlexibleInstances #-}

module HW08.Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import HW08.Employee
import HW08.Party

main :: IO ()
main = TF.defaultMain tests

tests :: [TF.Test]
tests = [ testGroup "QuickCheck HW08" [
--              testProperty "prop_nextLevel" prop_nextLevel
              ],
          testGroup "HUnit HW08" [
{-            let i = 3
                jl = (Append (Size 0) (Single (Size 1) 1) (Append (Size 1) (Append (Size 1) Empty (Single (Size 2) 2)) (Single (Size 2) 5)))::JoinList Size Int in
            testCase "indexJ" (indexJ i jl @=? jlToList jl !!? i)-}
            ]
        ]

--prop_nextLevel :: Employee -> [(GuestList, GuestList)] -> Bool
--prop_nextLevel e gls = let (wb, nb) = nextLevel e gls in


{-
instance Arbitrary (T.Tree Int) where
  arbitrary = frequency [ (1, t0)
                        , (5, T.Node <$> arbitrary <*> sequence [l, l]) ]
  where t3 = T.Node <$> arbitrary <*> sequence [arbitrary, arbitrary, arbitrary]
        t0 = T.Node <$> arbitrary <*> pure []
        l = frequency [ (1, t3), (5, t0) ]
-}
