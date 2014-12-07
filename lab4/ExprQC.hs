module ExprQC where

import Test.QuickCheck

prop_showReadExpr :: Expr -> Bool
prop_showReadExpr e = let s = showExpr e 
                          Just e' = readExpr s
                      in showExpr e' == s

prop_simplify :: Expr -> Bool
prop_simplify e = let e' = simplify e
                      e'' = simplify e'
                  in eval e 1 == eval e' 1 && e' == e''

arbExpr :: Int -> Gen Expr
arbExpr s = frequency [(2, rNum), (2, return X), (1, rFunc), (s, rOp)]
  where
    s' = s `div` 2

    rNum = do 
      n <- arbitrary
      return $ Num n

    rOp = do 
      op <- elements [Add, Mul]
      e1 <- arbExpr s'
      e2 <- arbExpr s'
      return $ op e1 e2

    rFunc = do 
      func <- elements [Sin, Cos]
      e <- arbExpr s'
      return $ func e

instance Arbitrary Expr where
  arbitrary = sized arbExpr