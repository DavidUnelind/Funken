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
exec [] _ output = output
exec (Assignment v e : statements) d input =
    let newD = Dictionary.insert (v, Expr.value e d) d
    in exec statements newD input
exec (Skip : statements) d input = exec statements d input
exec (Begin newS : statements) d input = exec (newS ++ statements) d input
exec (If cond thenStmts elseStmts: stmts) d input = 
    if (Expr.value cond d)>0 
    then exec (thenStmts: stmts) d input
    else exec (elseStmts: stmts) d input
exec (While cond body : statements) d input =
    if Expr.value cond d > 0
    then exec (body : While cond body : statements) d input
    else exec statements d input
exec (Read v : statements) d (i:input) =
    let newD = Dictionary.insert (v, i) d
    in exec statements newD input
exec (Write e : statements) d input =
    let outputValue = Expr.value e d
    in outputValue : exec statements d input
exec (Comment _ : statements) d input = exec statements d input

instance Parse Statement where
  parse = assignment ! skip ! begin ! ifStatement ! while ! readStatement ! write ! comment
  toString = error "Statement.toString not implemented"

tab n = replicate n '\t'

ts :: Int -> Statement -> String
ts n (Assignemnt v e) = tab n ++ v ++ " := " ++ Expr.toString e ++ ";\n"