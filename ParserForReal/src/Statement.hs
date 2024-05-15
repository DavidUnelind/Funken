module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T
    | Skip
    | Begin [Statement]
    | If Expr.T Statement Statement
    | While Expr.T Statement
    | Read String
    | Write Expr.T
    | Comment String
    deriving Show

assignment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
    where buildAss (v, e) = Assignment v e

skip :: Parser Statement
skip = accept "skip" #- require ";" >-> const Skip

begin :: Parser Statement
begin = accept "begin" -# iter parse #- require "end" >-> Begin

ifStatement :: Parser Statement
ifStatement = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> buildIf
    where buildIf ((cond, thenStmts), elseStmts) = If cond thenStmts elseStmts

while :: Parser Statement
while = accept "while" -# Expr.parse # require "do" -# parse >-> buildWhile
    where buildWhile (cond, body) = While cond body

readStatement :: Parser Statement
readStatement = accept "read" -# word #- require ";" >-> Read

write :: Parser Statement
write = accept "write" -# Expr.parse #- require ";" >-> Write

comment :: Parser Statement
comment = accept "--" -# iter (char ? (/= '\n')) #- require "\n" >-> Comment

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse = assignment ! skip ! begin ! ifStatement ! while ! readStatement ! write ! comment
  toString = error "Statement.toString not implemented"
