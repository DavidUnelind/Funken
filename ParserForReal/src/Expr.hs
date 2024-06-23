--{-# LANGUAGE InstanceSigs #-}
module Expr(Expr, T, parse, fromString, value, toString) where

import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
--import Data.Maybe (fromMaybe)

data Expr = Num Integer | Var String | Add Expr Expr 
       | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Pow Expr Expr 
        deriving Show

type T = Expr

var, num, factor, term, expr, pow :: Parser Expr

term', expr', pow' :: Expr -> Parser Expr

var = word >-> Var

num = number >-> Num

mulOp = lit '*' >-> (\_ -> Mul) !
        lit '/' >-> (\_ -> Div)

addOp = lit '+' >-> (\_ -> Add) !
        lit '-' >-> (\_ -> Sub)

powOp = lit '^' >-> const Pow

bldOp e (oper,e') = oper e e'

factor = num ! var ! lit '(' -# expr #- lit ')' ! err "illegal factor"

pow' e = powOp # pow >-> bldOp e #> pow' ! return e
pow = factor #> pow'

term' e = mulOp # pow >-> bldOp e #> term' ! return e
term = pow #> term'
       
expr' e = addOp # term >-> bldOp e #> expr' ! return e
expr = term #> expr'

parens :: Bool -> [Char] -> [Char] 
parens cond str = if cond then "(" ++ str ++ ")" else str

shw :: Int -> Expr -> String
shw prec (Num n) = show n
shw prec (Var v) = v
shw prec (Add t u) = parens (prec>5) (shw 5 t ++ "+" ++ shw 5 u)
shw prec (Sub t u) = parens (prec>5) (shw 5 t ++ "-" ++ shw 6 u)
shw prec (Mul t u) = parens (prec>6) (shw 6 t ++ "*" ++ shw 6 u)
shw prec (Div t u) = parens (prec>6) (shw 6 t ++ "/" ++ shw 7 u)
shw prec (Pow t u) = parens (prec>7) (shw 7 t ++ "^" ++ shw 8 u)

value :: Expr -> Dictionary.T String Integer -> Integer
value (Num n) _ = n
value (Var v) d = case Dictionary.lookup v d of 
        Nothing -> error ("undefined variable " ++ v)
        Just(v) -> v
value (Add a b) d = value a d + value b d
value (Sub a b) d = value a d - value b d
value (Mul a b) d = value a d * value b d
value (Div a b) d
        | value b d == 0 = error "division by 0"
        | otherwise = div (value a d) (value b d)
value (Pow a b) d = value a d ^ value b d

instance Parse Expr where
    parse = expr
    toString = shw 0
