module Analizzatore_sint_2 (
  prog,
  LKC (..),
  Exc (..),
  Exception
  ) where

import Lexer
import Prelude hiding (EQ,exp)

-- Exc type: can be either something (a) or an exception could be raised
data Exc a = Raise Exception | Return a
type Exception = String

instance Show a => Show (Exc a) where
  show (Raise e)  = "ERRORE: " ++ e
  show (Return x) = "COMPILATO " ++ (show x)

-- Exc is a monad, thus it is easier to control the flow of chained operations
instance Monad Exc where
  return x  = Return x
  (Raise e) >>= q   = Raise e
  (Return x) >>= q  = q x

raise :: Exception -> Exc a
raise e = Raise e

-- rec_x is a function which, given a list of token, returns a monad eventually
-- raising an exception if the input is not well-formed
rec_key :: [Token] -> Exc ([Token], (LKC -> [(LKC, LKC)] -> LKC))
rec_key ((Keyword LET):b)    = Return (b, LETC)
rec_key ((Keyword LETREC):b) = Return (b, LETRECC)
rec_key (a:b)                = Raise ("trovato " ++ show(a) ++
                                      ", atteso LET o LETREC")
rec_key  x                   = Raise ("ERRORE STRANO"  ++  show(x))

rec_in :: [Token] -> Exc [Token]
rec_in ((Keyword IN):b) = Return b
rec_in (a:b)            = Raise ("trovato " ++ show(a) ++ ", atteso IN")

rec_end :: [Token] -> Exc [Token]
rec_end ((Keyword END):b) = Return b
rec_end (a:b)             = Raise ("trovato " ++ show(a) ++ ", atteso END")

rec_then :: [Token] -> Exc[Token]
rec_then ((Keyword THEN):b) = Return b
rec_then (a:b)              = Raise ("trovato " ++ show(a) ++ ", atteso THEN")

rec_else :: [Token] -> Exc[Token]
rec_else ((Keyword ELSE):b) = Return b
rec_else (a:b)              = Raise ("trovato " ++ show(a) ++ ", atteso ELSE")

rec_lp :: [Token] -> Exc[Token]
rec_lp ((Symbol LPAREN):b) = Return b
rec_lp (a:b)               = Raise ("trovato " ++ show(a) ++ ", atteso ELSE")

rec_rp :: [Token] -> Exc[Token]
rec_rp ((Symbol RPAREN):b) = Return b
rec_rp (a:b)               = Raise ("trovato " ++ show(a) ++ ", attesa )")

rec_virg :: [Token] -> Exc[Token]
rec_virg ((Symbol VIRGOLA):b) = Return  b
rec_virg (a:b)                = Raise ("trovato " ++ show(a) ++ ", attesa ,")

rec_equals :: [Token] -> Exc[Token]
rec_equals ((Symbol EQUALS):b)= Return b
rec_equals (a:b)              = Raise ("trovato " ++ show(a) ++ ", atteso =")


data LKC = ETY | -- eps productions
           VAR String | NUM Integer |STRI String | BOO Bool |
           NIL | ADD LKC LKC | SUB LKC LKC | MULT LKC LKC |
           REM LKC LKC | DIV LKC LKC | EQC LKC LKC | LEQC LKC LKC |
           CARC LKC | CDRC LKC | CONSC LKC LKC | ATOMC LKC |
           IFC LKC LKC LKC | LAMBDAC [LKC] LKC | CALL LKC [LKC] |
           LETC LKC [(LKC,LKC)] | LETRECC LKC [(LKC, LKC)]
           deriving(Show, Eq)
--  remember: LET[REC]C takes two arguments:
--  * the former is the sequence of instructions that follow 'in'
--  * the latter is a list of binders (variables and respective values)


-- function that is exposed in the interface of the module
prog :: [Token] -> Exc LKC
prog input =
    case funProg input of
      Return (a, b) -> Return b
      Raise b -> Raise b

{- For every non-terminal N in grammar:
   * Add a pattern matching definition if the next token leads to a new
     distinct LKC production
   * Add a pattern matching definition if eps is in FIRST(N) and the next
     token T satisfy two conditions:
     - T is in FOLLOW(N)
     - T leads to a new distinct LKC production
 -}
-- production set #1 > Non-terminal Prog
funProg :: [Token] -> Exc ([Token], LKC)
funProg a = do
           (after_let, let_key) <- rec_key a                 -- let || letrec
           (after_bind, binders) <- funBind (after_let)      -- Bind (p#2)
           after_in <- rec_in after_bind                     -- in (syntax)
           (after_body, body) <- exp after_in                -- Exp (p#4)
           after_end <- rec_end after_body
           Return (after_end, let_key body binders)


-- production set #2 > Non-terminal Bind
funBind :: [Token] -> Exc ([Token], [(LKC,LKC)])
funBind ((Id a):b) =  do
                                x <- rec_equals b
                                (after_binder, value) <- exp x
                                (after_X, pairs) <- funX after_binder
                                Return (after_X, (VAR a, value):pairs)
funBind (a:_)      =  Raise ("BINDER CON "++ show(a) ++ " A SINISTRA")


-- production set #3 > Non-terminal X
funX :: [Token] -> Exc ([Token], [(LKC,LKC)])
funX (tokens) = case tokens of
  ((Keyword AND):b) -> funBind b  -- another Bind => p#2
  a@((Keyword IN):b) -> Return (a, [])  -- last Bind => return
  otherwise -> Raise ("DOPO BINDERS; TROVATO"++show(head(tokens)))


-- production set #4 > Non-terminal Exp
exp :: [Token] -> Exc ([Token], LKC)
exp a@((Keyword LET):b)    = funProg a  -- program => p#1
exp a@((Keyword LETREC):b) = funProg a  -- "recursive" program => p#1
exp ((Keyword LAMBDA):b)   = do           -- function declaration (p#4.2)
                          after_op_bracket <- rec_lp b
                          (after_argList, args) <- seq_var (after_op_bracket)
                          after_clos_bracket <- rec_rp after_argList
                          (after_lExp, lValue) <- exp after_clos_bracket
                          Return (after_lExp, LAMBDAC args lValue)
exp ((Operator CONS):b)    = do           -- list construction
                          after_op_bracket <- rec_lp b
                          (after_fst_exp, val1) <- exp after_op_bracket
                          after_comma <- rec_virg after_fst_exp
                          (after_snd_exp, val2) <- exp after_comma
                          after_clos_bracket <- rec_rp after_snd_exp
                          Return (after_clos_bracket, CONSC val1 val2)
exp ((Operator LEQ):b)     = do           -- less or equal operator
                          after_op_bracket <- rec_lp b
                          (after_fst_exp, val1) <- exp after_op_bracket
                          after_comma <- rec_virg after_fst_exp
                          (after_snd_exp, val2) <- exp after_comma
                          after_clos_bracket <- rec_rp after_snd_exp
                          Return (after_clos_bracket, LEQC val1 val2)
exp ((Operator EQ):b)      = do           -- comparing operator
                          after_op_bracket <- rec_lp b
                          (after_fst_exp, val1) <- exp after_op_bracket
                          after_comma <- rec_virg after_fst_exp
                          (after_snd_exp, val2) <- exp after_comma
                          after_clos_bracket <- rec_rp after_snd_exp
                          Return (after_clos_bracket, EQC val1 val2)
exp ((Operator CAR):b)      = do          -- first element of a list
                          after_op_bracket <- rec_lp b
                          (after_exp, lkcValue) <- exp after_op_bracket
                          after_clos_bracket <- rec_rp after_exp
                          Return (after_clos_bracket, CARC lkcValue)
exp ((Operator CDR):b)      = do          -- remainder of a list
                          after_op_bracket <- rec_lp b
                          (after_exp, lkcValue) <- exp after_op_bracket
                          after_clos_bracket <- rec_rp after_exp
                          Return (after_clos_bracket, CDRC lkcValue)
exp ((Operator ATOM):b)     = do          -- LISP atom
                          after_op_bracket <- rec_lp b
                          (after_exp, lkcValue) <- exp after_op_bracket
                          after_clos_bracket <- rec_rp after_exp
                          Return (after_clos_bracket, ATOMC lkcValue)
exp ((Keyword IF):b)        = do          -- if statement (p#4.3)
                          (after_cond, cond) <- exp b
                          after_then <- rec_then after_cond
                          (after_tBranch, tBranch) <- exp after_then
                          after_else <- rec_else after_tBranch
                          (after_fBranch, fBranch) <- exp after_else
                          Return (after_fBranch, IFC cond tBranch fBranch)
exp x                       =  funExpA x

-- production set #5 > Non-terminal ExpA
funExpA :: [Token] -> Exc ([Token], LKC)
funExpA a = do
           (after_T, firstTerm) <- funT a  -- T (p#7)
           funE1 (after_T, firstTerm)      -- E1 (p#6)


-- production set #6 > Non-terminal E1
funE1 :: ([Token], LKC) -> Exc ([Token], LKC)
funE1 ((Symbol PLUS):b, term)    = do  -- summation
                              (after_T, valT) <- funT b
                              funE1 (after_T, ADD term valT)
funE1 ((Symbol MINUS):b, term)   = do  -- subtraction
                              (after_T, valT) <- funT b
                              funE1 (after_T, SUB term valT)
funE1 (x, term)                  =     -- end of expression
                              Return (x, term)


-- production set #7 > Non-terminal T
funT :: [Token] -> Exc ([Token], LKC)
funT a = do
           (after_F, firstTerm) <- funF a
           funT1 (after_F, firstTerm)


-- production set #8 > Non-terminal T1
funT1 :: ([Token], LKC) -> Exc ([Token], LKC)
funT1 ((Symbol TIMES):b, term)    = do  -- multiplication
                              (after_F, valF) <- funF b
                              funT1 (after_F, MULT term valF)
funT1 ((Symbol DIVISION):b, term) = do  -- division
                              (after_F, valF) <- funF b
                              funT1 (after_F, DIV term valF)
funT1 (x, term)                   =
                              Return (x, term)


isExp_const::Token -> Bool
isExp_const (Number n)  = True
isExp_const Nil         = True
isExp_const (Bool _)    = True
isExp_const (String _)  = True
isExp_const  _          = False

get_value::Token -> LKC
get_value (Number n)  = NUM n
get_value Nil         = NIL
get_value (Bool b)    = BOO b
get_value (String s)  = STRI s


-- production set #9 > Non-terminal F
funF :: [Token] -> Exc ([Token], LKC)
funF (a:b)                 =
    if (isExp_const a) then
      Return (b, get_value a)  -- const_exp
    else
      fX (a:b)  -- var or exp


-- production set #9 (aux)
fX :: [Token] -> Exc ([Token], LKC)
fX ((Id variable):b)       =
                        funY (b, VAR variable)
fX ((Symbol LPAREN):b)     = do
                        (after_exp, expValue) <- funExpA b
                        after_clos_bracket <- rec_rp after_exp
                        Return (after_clos_bracket, expValue)
fX (a:_)                   =
                        Raise ("ERRORE in fX, TROVATO" ++ show(a))


-- production set #10 > Non-terminal Y
funY :: ([Token], LKC) -> Exc ([Token], LKC)
funY ((Symbol LPAREN):b, f)   =  do
                          (after_argList, argList) <- seq_exp b
                          after_clos_bracket <- rec_rp after_argList
                          Return (after_clos_bracket, CALL f argList)
funY (x, v)                   =
                          Return (x, v)


-- production set #14 > Non-terminal Seq_Exp
seq_exp :: [Token] -> Exc ([Token], [LKC])
seq_exp b@((Symbol RPAREN):_) =  -- end of sequence
                          Return (b, [])
seq_exp (a)                   = do
                          (after_exp, expValue) <- exp a
                          (after_args, args) <- nextArgs (after_exp)
                          Return (after_args, expValue:args)


-- production set #15 > Non-terminal Seq_Var
seq_var :: [Token] -> Exc ([Token], [LKC])
seq_var (endSeq@((Symbol RPAREN):b)) =
                          Return (endSeq, [])
seq_var ((Id variable):b) = do
                          (after_seq, list) <- seq_var b
                          Return (after_seq, (VAR variable):list)
seq_var (a:_) =
                          Raise ("ERRORE in seq_var, TROVATO " ++ show(a))


-- production set #16 > Non-terminal NextArgs
nextArgs :: [Token] -> Exc ([Token], [LKC])
nextArgs endSeq@((Symbol RPAREN):_) =     -- end of sequence
                                Return (endSeq, [])
nextArgs tkns@((Symbol VIRGOLA):_)  = do  -- parse next sequences
                                after_comma <- rec_virg tkns
                                (after_seq, args) <- seq_exp (after_comma)
                                Return (after_seq,args)
nextArgs a                          =
                      Raise ("ERRORE in nextArgs, TROVATO " ++ show(a))


-- examples

--right
c = "letrec  FACT = lambda ( X ) if  eq ( X, 0 ) then 1 else  X*FACT(  X- 1 )and G = lambda ( H L ) if  eq ( L,  nil ) then L else cons( H(car( L ) ), G ( H, cdr ( L ) )) in G ( FACT, cons(1, cons(2, cons(3, nil))) ) end $";

-- right
d = "let x=cons(\"ab\", cons(\"cd\", nil)) in if true then cons(\"01\", x) else nil end $";

-- right
myFun2 = "let a = 2 in lambda ( Y Z) a * 3 end $"

-- right
factorial = "let prod = lambda (N) 2 in prod(5) end $"

-- wrong
myFun = "let \"a b\" x = (~2) 3 52 $";

tpnt19 = "let x= lambda (a) a in x ( car(cons(1, nil)) ) end $";
asdf = lexi tpnt19
