module Expr where

import Parsing
import Data.Char
import Data.Maybe

data Expr
  = Num   Double
  | Bin   String Expr Expr
  | Func  String Expr
  | X
  deriving Eq

-- Produce a nice string representation of an expression.
showExpr :: Expr -> String
showExpr X          = "x"
showExpr (Num n)    = show n
showExpr (Bin "+" e e') = showExpr e ++ " + " ++ showExpr e'
showExpr (Bin "*" e e') = showFactor e ++ " * " ++ showFactor e'
showExpr (Func s e)     = s ++ showFunc e

-- Only add parenthesis when we need them.
showFactor e@(Bin "+" _ _)  = addParenthesis e
showFactor e                = showExpr e

-- Only add parenthesis when we need them.
showFunc e@(Bin "*" _ _) = addParenthesis e
showFunc e = showFactor e

addParenthesis e = "(" ++ showExpr e ++ ")"

instance Show Expr where
  show = showExpr

-- Given an expression and a value for x, calculate the result
eval :: Expr -> Double -> Double
eval X          x = x
eval (Num n)    _ = n
eval (Bin "+" e e') x = eval e x + eval e' x
eval (Bin "*" e e') x = eval e x * eval e' x
eval (Func "sin" e)    x = sin $ eval e x
eval (Func "cos" e)    x = cos $ eval e x

-- ### PARSING

other :: Parser Expr
other = func +++ var +++ num

func :: Parser Expr
func =  parseString "sin" >-> (pmap (Func "sin") $ factor) +++ 
        parseString "cos" >-> (pmap (Func "cos") $ factor)

parseString :: [Char] -> Parser String
parseString [] = success "success"
parseString (x:xs) = char x >-> parseString xs

var :: Parser Expr
var = char 'x' >-> success X

num :: Parser Expr
num = pmap Num $ oneOrMore (sat (`elem` ".-0123456789")) >*> success.read
-- num = pmap Num $ float +++ pmap negate (char '-' >-> float)
--   where float = oneOrMore digit >*> \ds -> ((char '.' >-> oneOrMore digit >*> \dss -> success (read (ds ++ '.':dss))) +++ success (read ds))

expr    = foldr1 (Bin "+") `pmap` chain term (char '+')
term    = foldr1 (Bin "*") `pmap` chain factor (char '*')
factor  = char '(' >-> expr <-< char ')' +++ other

readExpr :: String -> Maybe Expr
readExpr s =  let s' = filter (not.isSpace) s in
                case parse expr s' of
                  Just (e, "") -> Just e
                  _            -> Nothing

-- ### END PARSING

-- Given an expression, simplify it
simplify :: Expr -> Expr
simplify (Bin "+" e e') = add (simplify e) (simplify e')
simplify (Bin "*" e e') = mul (simplify e) (simplify e')
simplify e          = e

add (Num n) (Num m) = Num (n + m)
add (Num 0) e       = e
add e       (Num 0) = e
add e1      e2      = Bin "+" e1 e2

mul (Num n) (Num m) = Num (n * m)
mul (Num 0) e       = Num 0
mul e       (Num 0) = Num 0
mul (Num 1) e       = e
mul e       (Num 1) = e
mul e1      e2      = Bin "*" e1 e2

-- Given an expression, differentiate it
differentiate :: Expr -> Expr
differentiate e = simplify $ differentiate' e
  where
    differentiate' :: Expr -> Expr
    differentiate' (Bin "+" e1 e2)  = add (differentiate' e1) (differentiate' e2)
    differentiate' (Bin "*" e1 e2)  = add (mul (differentiate' e1) e2) (mul e1 (differentiate' e2))
    differentiate' (Func "sin" e)      = Bin "*" (differentiate' e) (Func "cos" e)
    differentiate' (Func "cos" e)      = Bin "*" (Num (-1)) (Func "sin" e)
    differentiate' X            = Num 1
    differentiate' _            = Num 0

type Point = (Double, Double)

-- Given an expression, a scaling factor and the height and width for a plotting canvas, 
-- calculate the resulting set of points 
points :: Expr -> Double -> (Int, Int) -> [Point]
points e s (w, h) = [(px, (-(eval e ((px - w' * 0.5) * s)) / s) + h' * 0.5) | px <- [0..w']]
  where w'  = fromIntegral w
        h'  = fromIntegral h