{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

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

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- ex2
attMaxForce :: Int
attMaxForce = 3

attMinLeaveBehind:: Int
attMinLeaveBehind = 1

defMaxForce :: Int
defMaxForce = 2

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield atts defs) = let aForce = min attMaxForce (atts - attMinLeaveBehind)
                                     dForce = min defMaxForce defs
                                     attack force = (sequence . replicate force) die >>= \ds -> return $ (reverse . sort) ds
                                     match = sequence [attack aForce, attack dForce] >>=
                                             \[aFight, dFight] -> return $ foldr calcFight (atts, defs) . zip aFight $ dFight
                                     calcFight (aB, dB) (a, d) = if aB > dB then (a, pred d) else (pred a, d) in
                                 match >>= \(aFinal, dFinal) -> return $ Battlefield aFinal dFinal

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield atts defs) = undefined
