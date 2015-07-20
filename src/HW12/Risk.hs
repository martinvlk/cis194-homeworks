{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random (Random(..), StdGen, Rand, getRandom, randomR, evalRandIO)
import Control.Monad (replicateM)
import Data.List (sortBy)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)

-- ex2
attMaxForce :: Int
attMaxForce = 3

attMinLeaveBehind:: Int
attMinLeaveBehind = 1

defMaxForce :: Int
defMaxForce = 2

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield atts defs) = return . uncurry Battlefield =<< fight
   where fight = return . foldr move (atts, defs) . uncurry zip . (\[a, b]->(a,b))
                 =<< sequence [dice aForce, dice dForce]
         move (aB, dB) (a, d) = if aB > dB then (a, pred d) else (pred a, d)
         dice n = return . sortBy (flip compare) =<< replicateM n die
         aForce = min attMaxForce (atts - attMinLeaveBehind)
         dForce = min defMaxForce defs

-- ex3

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield atts defs) | atts >= 2 && defs > 0 = invade =<< battle bf
                                  | otherwise = return bf

-- ex4
sampleSize :: Int
sampleSize = 1000

successProb :: Battlefield -> Rand StdGen Double
successProb bf = return . (/fromIntegral sampleSize) . sum . map aWin
                 =<< replicateM sampleSize (invade bf)
  where aWin (Battlefield atts defs) = if atts > defs then 1 else 0

main :: IO ()
main = print =<< evalRandIO (successProb $ Battlefield 300 300)
