{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import qualified StackVM as VM
import qualified Data.Map as M

-- ex1
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- ex2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

-- ex3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

-- ex4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = lit $ max a b
  mul (MinMax a) (MinMax b) = lit $ min a b

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 a) (Mod7 b) = lit $ a+b
  mul (Mod7 a) (Mod7 b) = lit $ a*b

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"

-- ex5
instance Expr VM.Program where
  lit n = [VM.PushI n]
  add es1 es2 = concat [es2, es1, [VM.Add]]
  mul es1 es2 = concat [es2, es1, [VM.Mul]]

-- the above doesn't allow Bool types, so we can never get
-- badly typed values in the emitted assmebly

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

-- parseExp lit add mul "(3*6)+4" :: Maybe Program
-- compile "(3*6)+4"
-- VM.stackVM $ fromMaybe [] $ compile "(3*6)+4"

-- ex6
class HasVars a where
  var :: String -> a

data VarExprT = Lit' Integer
              | Add' VarExprT VarExprT
              | Mul' VarExprT VarExprT
              | Var String
              deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit'
  add = Add'
  mul = Mul'

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n _ = Just n
  add a b m = (+) <$> a m <*> b m
  mul a b m = (*) <$> a m <*> b m

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer
withVars vs e = e $ M.fromList vs
