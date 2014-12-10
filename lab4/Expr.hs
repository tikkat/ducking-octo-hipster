module Expr where

import Test.QuickCheck
import Parsing

data Expr = Num Double
          | Sin Expr
          | Cos Expr
          | Add Expr Expr
          | Mul Expr Expr
          | Var

showExpr :: Expr -> String
showExpr Var = "x"
showExpr (Num n)    = show n
showExpr (Sin n)    = "sin (" ++ showExpr n ++ ")"
showExpr (Cos n)    = "cos (" ++ showExpr n ++ ")"
showExpr (Add a b)  = showExpr a ++ "+" ++ showExpr b
showExpr (Mul a b)  = showFactor a ++ "*" ++ showFactor b

showFactor :: Expr -> String
showFactor (Add a b)  = "("++showExpr (Add a b)++")"
showFactor e          = showExpr e

instance Show Expr where
  show = showExpr

eval :: Expr -> Double -> Double
eval Var x        = x
eval (Num n) _    = n
eval (Sin e) x    = sin $ eval e x
eval (Cos e) x    = cos $ eval e x
eval (Add a b) x  = (eval a x) + (eval b x)
eval (Mul a b) x  = (eval a x) * (eval b x)

readExpr :: String -> Maybe Expr
readExpr = undefined

sin' :: Expr -> Double
sin' = undefined

cos' :: Expr -> Double
cos' = undefined
