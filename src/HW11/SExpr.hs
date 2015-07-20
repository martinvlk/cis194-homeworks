{- CIS 194 HW 11
   due Monday, 8 April
-}

module HW11.SExpr where

import HW10.AParser
import Control.Applicative hiding ((*>))
import Data.Char

(*>) :: Applicative f => f a -> f b -> f b
a *> b = pure (const id) <*> a <*> b

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f = foldr (liftA2 (:) . f) $ pure []

sequenceA  :: Applicative f => [f a] -> f [a]
sequenceA = mapA id

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n a = pure (replicate n) <*> a

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser Ident
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

atom :: Parser Atom
atom = N <$> posInt <|> I <$> ident

ignSpaces :: Parser a -> Parser a
ignSpaces p = spaces *> p <* spaces

parseSExpr :: Parser SExpr
parseSExpr = A <$> ignSpaces atom <|>
             Comb <$> ignSpaces (char '(') *>
             ignSpaces (oneOrMore parseSExpr) <* ignSpaces (char ')')
