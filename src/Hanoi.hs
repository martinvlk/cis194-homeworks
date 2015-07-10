module Hanoi where

import qualified Data.Map as HM
import qualified Data.Maybe as M
--import qualified Debug.Trace as D

{- Zadání:
To move n discs (stacked in increasing size) from peg
a to peg b using peg c as temporary storage,
1. move n-1 discs from a to c using b as a temporary storage
2. move the top disc from a to b
3. move n-1 discs from c to b using a as a temporary storage

Given the number of discs and names for the three pegs, hanoi
should return a list of moves to be performed to move the stack of
discs from the first peg to the second.
-}

data Peg = P1 | P2 | P3 | P4 deriving (Eq, Ord, Show)
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
                      -- base case, když už máme jen jeden disk, můžeme
                      -- ho bez dalšího přesunout na cílový kolík 
hanoi 1 ini tgt _   = [(ini, tgt)]
hanoi n ini tgt tmp = -- odložit n-1, pro další úroveň prohodí tgt s tmp,
                      -- takže naším novým cílem pro n-1 je tmp
                      hanoi (n-1) ini tmp tgt
                      -- umístit zbývající disk -- směřuje k momentálnímu
                      -- cíli na stávájící úrovni
                      ++ hanoi    1  ini tgt nil
                      -- restart, pro další úroveň - ponechá tgt, ale prohodí
                      -- ini s tmp, protože tam jsme si výše odložili disky
                      ++ hanoi (n-1) tmp tgt ini

nil :: a
nil = undefined

{-
V každém rekurzívním kroku se určitým způsobem zamění výchozí (ini),
cílový (tgt) a odkládací (tmp) kolík, takže nový krok má odlišný
pohled na situaci.

Rekurze je několikanásobná, v jednom kroku voláme 3x sami sebe.

Jak ten algoritmus zaručí, že řešení směřuje k umístění na určený
cílový kolík, když se pohled mění v každém kroku?

Musí to spočívat v jeho systematičnosti, je to tam implicitně
zakódované. Ale jak?

Je to dáno tím, že jednotlivé rekurzívní kroky na dané úrovni
probíhají jeden za druhým a vždy se nejdříve odloží n-1 na tmp, pak se
zbývající disk umístí na tgt. Třetí rekurzívní volání pak celý postup
zopakuje pro úroveň n-1 s tím, že tgt zůstává pořád stejný. Díky tomu
celý postup směřuje k přesunu na kolík tgt.

Zkusím si projít vyhodnocení pro malý počet disků, řekněme 5.
hanoi 5     p1 p3 p2 -- chceme se dostat z p1 na p3 a požít p2 na odkládání
  hanoi 4    p1 p2 p3 -- při postupu na nižší úroveň se prohodí 2 a 3
    hanoi 3   p1 p3 p2 -- a znovu...
      hanoi 2  p1 p2 p3 -- atd.
        hanoi 1 p1 p3 p2 -- když je n=1, tak teprve začneme přesunovat disky, 1>>3
        hanoi 1 p1 p2  _ -- 1>>2
        hanoi 1 p3 p2 p1 -- 3>>2
      hanoi 1  p1 p3  _ -- 1>>3
      hanoi 2  p2 p3 p1
        hanoi 1 p2 p1 p3 -- 2>>1
        hanoi 1 p2 p3  _ -- 2>>3
        hanoi 1 p1 p3 p2 .. 1>>3
    hanoi 1   p1 p2  _ -- 1>>2
    hanoi 3   p3 p2 p1
      hanoi 2  p3 p1 p2
        hanoi 1 p3 p2 p1 -- 3>>2
        hanoi 1 p3 p1  _ -- 3>>1
        hanoi 1 p2 p1 p3 -- 2>>1
      hanoi 1  p3 p2  _ -- 3>>2
      hanoi 2  p1 p2 p3
        hanoi 1 p1 p3 p2 -- 1>>3
        hanoi 1 p1 p2  _ -- 1>>2
        hanoi 1 p3 p2 p1 -- 3>>2
  hanoi 1    p1 p3  _ -- 1>>3
  hanoi 4    p2 p3 p1
    hanoi 3   p2 p1 p3
      hanoi 2  p2 p3 p1
        hanoi 1 p2 p1 p3 -- 2>>1
        hanoi 1 p2 p3  _ -- 2>>3
        hanoi 1 p1 p3 p2 -- 1>>3
      hanoi 1  p2 p1  _ -- 2>>1
      hanoi 2  p3 p1 p2
        hanoi 1 p3 p2 p1 -- 3>>2
        hanoi 1 p3 p1  _ -- 3>>1
        hanoi 1 p2 p1 p3 -- 2>>1
    hanoi 1   p2 p3  _ -- 2>>3
    hanoi 3   p1 p3 p2
      hanoi 2  p1 p2 p3
        hanoi 1 p1 p3 p2 -- 1>>3
        hanoi 1 p1 p2  _ -- 1>>2
        hanoi 1 p3 p2 p1 -- 3>>2
      hanoi 1  p1 p3  _ -- 1>>3
      hanoi 2  p2 p3 p1
        hanoi 1 p2 p1 p3 -- 2>>1
        hanoi 1 p2 p3  _ -- 2>>3
        hanoi 1 p1 p3 p2 -- 1>>3

Je důležité si uvědomit, že jediné kroky, které opravdu přesunují
disky jsou ty, které pracují s n=1. Všechny ostatní jen popisují na
vyšší úrovni co se bude později dělat.

Zaručuje algoritmus to, že se nepoloží větší disk na menší? Pokud ano,
jak to dělá?

První krok na dané úrovni odloží n-1 disků na tmp. V tu chvíli jsou
buď ostatní kolíky prázdné, nebo obsahují jen větší disky, takže
pravidlo je zaručeno.

Další krok přesouvá největší disk na tgt, jelikož všechny ostatní
(menší) disky jsme přesunuli na tmp, tak tgt je buď prázdný nebo
obsahuje větší disk. Pravidlo zaručeno.

Poslední krok opět přesouvá n-1 disků, tentorát z tmp na tgt za použití
ini na odkládání. Všechny větší disky jsme už umístili na tgt, takže
pravidlo je opět zaručeno.
-}


{- ToH4 Frame-Stewart algorithm
 - source: https://en.wikipedia.org/wiki/Tower_of_Hanoi#Frame.E2.80.93Stewart_algorithm
 -}
hanoi4 :: Integer -> Peg -> Peg -> [Peg] -> [Move]
hanoi4 1 ini tgt _             =    [(ini, tgt)]
hanoi4 n ini tgt (tmp1:tmp2:_) =    hanoi4 k     ini  tmp1 [tmp2, tgt]
                                 ++ hanoi4 (n-k) ini  tgt  [tmp2]
                                 ++ hanoi4 k     tmp1 tgt  [tmp2, ini]
  where k = fromInteger n - (floor ((sqrt (2*fromInteger n)) + (1/2)))
        -- source: Corollary 3.3 in https://www2.bc.edu/~grigsbyj/Rand_Final.pdf
hanoi4 n ini tgt (tmp1:_) = hanoi n ini tgt tmp1
hanoi4 _ _ _ _ = []

{- Verifikace výsledku.
-> výchozí stav
-> brát jeden krok za druhým a generovat další stav
-> přitom ověřovat, že stav je konzistentní
-> je to valstně fold
-}

type Disc = Int
type Hanoi4State = HM.Map Peg [Disc]

validate :: [Move] -> Hanoi4State -> Either String Hanoi4State
validate [] state = Right state
validate (m@(pFrom, pTo):ms) state | validMove = validate ms makeMove
                                   | otherwise = Left ("Invalid move: " ++ show m ++ "; state " ++ show state)
  where validMove = M.isNothing dTo || dFrom < dTo
        dFrom = M.listToMaybe $ HM.findWithDefault [] pFrom state
        dTo   = M.listToMaybe $ HM.findWithDefault [] pTo   state
        makeMove = HM.adjust (M.fromJust dFrom:) pTo $ HM.adjust (drop 1) pFrom state

doValidate :: Either String Hanoi4State
doValidate = validate (hanoi4 15 P1 P4 [P2, P3]) initialState

doValidateFail :: Either String Hanoi4State
doValidateFail = validate [(P1, P2), (P1, P2)] initialState

initialState :: Hanoi4State
initialState = HM.fromList [(P1, [1..15]), (P2, []), (P3, []), (P4, [])]
