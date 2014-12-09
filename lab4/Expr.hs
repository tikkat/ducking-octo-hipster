module Expr where

import Parsing
import Data.Char

--import ExprQC

data Expr
  = Num Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sin Expr
  | Cos Expr
  | X
  deriving Eq

-- Can we add abs, pow and/or ^, sqrt

showExpr :: Expr -> String
showExpr X          = "x"
showExpr (Num n)    = show n
showExpr (Add e e') = showExpr e ++ " + " ++ showExpr e'
showExpr (Mul e e') = showFactor e ++ " * " ++ showFactor e'
showExpr (Sin e)    = "sin " ++ showFunc e
showExpr (Cos e)    = "cos " ++ showFunc e

showFactor (Add e e') = "(" ++ showExpr (Add e e') ++ ")"
showFactor e          = showExpr e

showFunc (Add e e') = "(" ++ showExpr (Add e e') ++ ")"
showFunc (Mul e e') = "(" ++ showExpr (Mul e e') ++ ")"
showFunc e          = showExpr e

instance Show Expr where
  show = showExpr

eval :: Expr -> Double -> Double
eval X          var = var
eval (Num n)    _   = n
eval (Add e e') var = eval e var + eval e' var
eval (Mul e e') var = eval e var * eval e' var
eval (Sin e)    var = sin $ eval e var
eval (Cos e)    var = cos $ eval e var

-- PARSER

other :: Parser Expr
other = sin' +++ cos' +++ var +++ num

sin' :: Parser Expr
sin' = pmap Sin $ char 's' >-> char 'i' >-> char 'n' >-> factor

cos' :: Parser Expr
cos' = pmap Cos $ char 'c' >-> char 'o' >-> char 's' >-> factor

num :: Parser Expr                                      -- Possible to use: reads "The string" :: [(Double, String)], here? 
num = pmap Num $ nat >*> \ds -> decimals >*> \decs -> success $ read $ ds ++ '.':decs
  where
    nat = pos +++ (char '-' >-> neg)
    pos = oneOrMore digit
    neg = oneOrMore digit >*> \ds -> success $ '-':ds
    decimals  = decimals' +++ success "0"
    decimals' = char '.' >-> oneOrMore digit

var :: Parser Expr
var = char 'x' >-> success X

expr    = foldr1 Add `pmap` chain term (char '+')
term    = foldr1 Mul `pmap` chain factor (char '*')
factor  = char '(' >-> expr <-< char ')' +++ other

readExpr :: String -> Maybe Expr
readExpr s =  let s' = filter (not.isSpace) s
              in case parse expr s' of
                Just (e, "") -> Just e
                _            -> Nothing

-- PARSER

simplify :: Expr -> Expr
simplify (Add e e') = add (simplify e) (simplify e')
simplify (Mul e e') = mul (simplify e) (simplify e')
simplify e          = e

add (Num n) (Num m) = Num (n + m)
add (Num 0) e       = e
add e       (Num 0) = e
-- add X       X       = Mul (Num 2) X                        -- #### WANTED?
add e1      e2      = Add e1 e2

mul (Num n) (Num m) = Num (n * m)
mul (Num 0) e       = Num 0
mul e       (Num 0) = Num 0
mul (Num 1) e       = e
mul e       (Num 1) = e
mul e1      e2      = Mul e1 e2

differentiate :: Expr -> Expr
differentiate e = simplify $ differentiate' e
  where
    differentiate' :: Expr -> Expr
    differentiate' (Add e1 e2)  = add (differentiate' e1) (differentiate' e2)
    differentiate' (Mul e1 e2)  = add (mul (differentiate' e1) e2) (mul e1 (differentiate' e2))
    differentiate' (Sin e)      = Mul (differentiate' e) (Cos e)
    differentiate' (Cos e)      = Mul (Num (-1)) (Sin e)
    differentiate' X            = Num 1
    differentiate' _            = Num 0

type Point = (Double, Double)

points :: Expr -> Double -> (Int, Int) -> [Point]
points e s (w, h) = [(px, (-(eval e ((px - w' * 0.5) * s)) / s) + h' * 0.5) | px <- [0..w']]
  where w'  = fromIntegral w
        h'  = fromIntegral h

{-FROM FORMULA:
y = eval exp x
x = (px - width * 0.5) * scale
py = (-y / scale) + height * 0.5-}