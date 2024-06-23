{- Test for Expr-}
module TestExpr where

import qualified Dictionary
import Expr

dict = Dictionary.insert ("x", 1) $
       Dictionary.insert ("y", 2) $
       Dictionary.empty 

testValue string = value (fromString string) dict

n1 = testValue "1" -- 1
n2 = testValue "x" -- 1
n3 = testValue "x+y" -- 3
n4 = testValue "x-y-y" -- -3
n21 = testValue "1/(2-y)" {-  Expr.value: division by 0 -}
n31 = testValue "2+z"     {-  Expr.value: undefined variable z -}

-- testing exponentiation

p1 = testValue "y^3" -- 8
p2 = testValue "y^3^4" -- 2417851639229258349412352
p3 = testValue "8^4" -- 4096
p4 = testValue "(y+3)*2^(x+y)" -- 40
p5 = testValue "2*3^4*2^5+3^2" -- 5193



