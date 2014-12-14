module Expr where

import Parsing
import Data.Char
import Data.Maybe

--import ExprQC

data Expr
  = Num   Double
  | Add   Expr Expr
  | Mul   Expr Expr
  | Pow   Expr Expr
  | Sin   Expr
  | Cos   Expr
  | Abs   Expr
  | Sqrt  Expr
  | X
  deriving Eq

showExpr :: Expr -> String
showExpr X          = "x"
showExpr (Num n)    = show n
showExpr (Add e e') = showExpr e ++ " + " ++ showExpr e'
showExpr (Mul e e') = showFactor e ++ " * " ++ showFactor e'
showExpr (Pow e e') = "(" ++ showFunc e ++ "^" ++ showFunc e' ++ ")"
showExpr (Sin e)    = "sin "  ++ showFunc e
showExpr (Cos e)    = "cos "  ++ showFunc e
showExpr (Abs e)    = "abs "  ++ showFunc e
showExpr (Sqrt e)   = "sqrt " ++ showFunc e

showFactor (Add e e') = "(" ++ showExpr (Add e e') ++ ")"
showFactor e          = showExpr e

showFunc (Add e e') = "(" ++ showExpr (Add e e') ++ ")"
showFunc (Mul e e') = "(" ++ showExpr (Mul e e') ++ ")"
showFunc e          = showExpr e

instance Show Expr where
  show = showExpr

eval :: Expr -> Double -> Double
eval X          x = x
eval (Num n)    _ = n
eval (Add e e') x = eval e x + eval e' x
eval (Mul e e') x = eval e x * eval e' x
eval (Pow e e') x = eval e x ** eval e' x
eval (Sin e)    x = sin $ eval e x
eval (Cos e)    x = cos $ eval e x
eval (Abs e)    x = abs $ eval e x
eval (Sqrt e)   x = sqrt $ eval e x

-- PARSER

other :: Parser Expr
other = sin' +++ cos' +++ abs' +++ sqrt' +++ var +++ num

sin' :: Parser Expr
sin' = pmap Sin $ char 's' >-> char 'i' >-> char 'n' >-> factor

cos' :: Parser Expr
cos' = pmap Cos $ char 'c' >-> char 'o' >-> char 's' >-> factor

abs' :: Parser Expr
abs' = pmap Abs $ char 'a' >-> char 'b' >-> char 's' >-> factor

sqrt' :: Parser Expr
sqrt' = pmap Sqrt $ char 's' >-> char 'q' >-> char 'r' >-> char 't' >-> factor

var :: Parser Expr
var = char 'x' >-> success X

num :: Parser Expr
num = pmap Num $ oneOrMore (sat $ \d -> d `elem` ".-0123456789") >*> \ds -> success $ read ds

expr    = foldr1 Add `pmap` chain term (char '+')
term    = foldr1 Pow `pmap` chain term' (char '^')
term'    = foldr1 Mul `pmap` chain factor (char '*')
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
    differentiate' (Abs e)      = undefined                    -- ### HOW SHOULD WE DO HERE SENSE D/DX ABS X = X / ABS X ?
    differentiate' (Sqrt e)     = undefined                    -- ### HOW SHOULD WE DO HERE SENSE D/DX SQRT X = 1 / 2 SQRT X ?
    differentiate' (Pow e1 e2)  = undefined                    -- ### HOW SHOULD WE DO HERE? DELETE POW ALL TOGETHER?
    differentiate' X            = Num 1
    differentiate' _            = Num 0

type Point = (Double, Double)

points :: Expr -> Double -> (Int, Int) -> [Point]
points e s (w, h) = [(px, (-(eval e ((px - w' * 0.5) * s)) / s) + h' * 0.5) | px <- [0..w']]
  where w'  = fromIntegral w
        h'  = fromIntegral h

{- FROM FORMULA:
y = eval exp x
x = (px - width * 0.5) * scale
py = (-y / scale) + height * 0.5 -}