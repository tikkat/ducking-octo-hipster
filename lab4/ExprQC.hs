module ExprQC where

import Test.QuickCheck
import Expr

-- Check that first showing and then reading an expression produce 
-- "the same" result as the expression we started with.
prop_showReadExpr :: Expr -> Bool
prop_showReadExpr e = let s = showExpr e 
                          Just e' = readExpr s
                      in showExpr e' == s

-- Check that the simplifier is correct (e.g. 1+1 does not simplify to 3), 
-- and that it simplifies as much as possible.
prop_simplify :: Expr -> Bool
prop_simplify e = let e'  = simplify e
                      e'' = simplify e'
                  in eval e 1 == eval e' 1 && e' == e''

arbExpr :: Int -> Gen Expr
arbExpr s = frequency [(2, rNum), (2, return X), (1, rFunc), (s, rOp)]
  where
    s' = s `div` 2

    rNum = do 
      n <- choose (-10000000, 10000000)
      return $ Num n

    rOp = do 
      op <- elements [Bin "+", Bin "*"]
      e1 <- arbExpr s'
      e2 <- arbExpr s'
      return $ op e1 e2

    rFunc = do 
      func <- elements [Func "sin", Func "cos"]
      e <- arbExpr s'
      return $ func e

instance Arbitrary Expr where
  arbitrary = sized arbExpr