module Analizzatore_sint_1(
  progdoll
  ) where

import Lexer
import Prelude hiding (EQ,exp)

data Exc a = Raise Exception | Return a
type Exception = String

instance Show a => Show (Exc a) where
  show (Raise e)  = "ERRORE:" ++ e
  show (Return x) = "RAGGIUNTO:" ++ (show x)

instance Monad Exc where
  return x  = Return x
  (Raise e) >>= q   = Raise e
  (Return x) >>= q  = q x

raise :: Exception -> Exc a
raise e = Raise e

-- rec_x is a function which, given a list of token, returns a monad eventually
-- raising an exception if the input is not well-formed
rec_key :: [Token] -> Exc [Token]
rec_key ((Keyword LET):b)    = Return b
rec_key ((Keyword LETREC):b) = Return b
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

rec_rp ((Symbol RPAREN):b) = Return b
rec_rp (a:b)               = Raise ("trovato " ++ show(a) ++ ", attesa )")

rec_virg ((Symbol VIRGOLA):b) = Return  b
rec_virg (a:b)                = Raise ("trovato " ++ show(a) ++ ", attesa ,")

rec_equals ((Symbol EQUALS):b)= Return b
rec_equals (a:b)              = Raise ("trovato " ++ show(a) ++ ", atteso =")


progdoll :: [Token] -> String
progdoll x = show (funProg x)


-- production #1
funProg :: [Token] -> Exc [Token]
funProg a = do
         x <- rec_key a  -- let || letrec (syntax)
         y <- funBind x  -- Bind (p#2)
         z <- rec_in y   -- in (syntax)
         w <- exp z      -- Exp (p#4)
         rec_end w       -- end (syntax)


-- production #2
funBind ((Id a):b)            =  do
                                x <- rec_equals b -- variable is equal to..
                                y <- exp x        -- Exp (p#4)
                                funX y            -- X (p#3)
funBind (a:_)                  = Raise ("BINDER CON "++ show(a) ++
                                        " A SINISTRA")


-- production #3
funX ((Keyword AND):b)     = funBind b  -- if there is another Bind => p#2
funX a@((Keyword IN):b)    = Return a   -- last bind => return
funX (a:_)                 = Raise ("DOPO BINDERS; TROVATO"++show(a))


-- production #4
exp :: [Token] -> Exc [Token]
exp a@((Keyword LET):b)    = (funProg a)  -- program => p#1
exp a@((Keyword LETREC):b) = (funProg a)  -- "recursive" program => p#1
exp ((Keyword LAMBDA):b)   = do           -- function declaration (p#4.2)
                                x <- seq_var b  -- decode variables sequence
                                exp x           -- decode expression
exp ((Operator CONS):b)    = do           -- list construction (p#4.3)
                                x <- rec_lp b    -- recognize left bracket
                                y <- exp x       -- parse first element
                                z <- rec_virg y  -- recognize comma
                                w <- exp z       -- parse second element
                                rec_rp w         -- recognize right bracket
exp ((Operator LEQ):b)     = do           -- less or equal operator (p#4.3)
                                x <- rec_lp b    -- recognize left bracket
                                y <- exp x       -- parse first element
                                z <- rec_virg y  -- recognize comma
                                w <- exp z       -- parse second element
                                rec_rp w         -- recognize right bracket
exp ((Operator EQ):b)      = do           -- comparing operator
                                x <- rec_lp b    -- recognize left bracket
                                y <- exp x       -- parse first element
                                z <- rec_virg y  -- recognize comma
                                w <- exp z       -- parse second element
                                rec_rp w         -- recognize right bracket
exp ((Operator CAR):b)      = exp b       -- first element of a list
exp ((Operator CDR):b)      = exp b       -- remainder of a list
exp ((Operator ATOM):b)     = exp b       -- LISP atom
exp ((Keyword IF):b)        = do          -- if statement (p#4.3)
                                x <- exp b       -- parse condition expression
                                y <- rec_then x  -- recognize then
                                z <- exp y       -- parse "true" branch
                                w <- rec_else z  -- recognize else
                                exp w            -- parse "false" branch
exp x                       =  funExpA x  -- LISP atom


-- production #5
funExpA :: [Token] -> Exc [Token]
funExpA a = do
           x <- funT a  -- T (p#7)
           funE1 x      -- E1 (p#6)


-- production #6
funE1 :: [Token] -> Exc [Token]
funE1 ((Symbol PLUS):b)    = do  -- summation
                              x <- funT b  -- extract terminal
                              funE1 x      -- recursive call to base case
funE1 ((Symbol MINUS):b)   = do  -- subtraction
                              x <- funT b  -- extract terminal
                              funE1 x      -- recursive call to base case
funE1 x                    = Return x  -- nothing


-- production #7
funT a = do
           x <- funF a  -- F  (p#9)
           funT1 x      -- T1 (p#8)


-- production #8
funT1 ((Symbol TIMES):b)   = do  -- multiplication
                              x <- funF b  -- extract terminal
                              funT1 x      -- recursive call to base case
funT1 ((Symbol DIVISION):b)= do  -- division
                              x <- funF b  -- extract terminal
                              funT1 x      -- recursive call to base case
funT1 x                    = Return x


-- exp_const tells if a token is a constant expression
exp_const::Token ->Bool
exp_const (Number _)  = True
exp_const Nil         = True
exp_const (Bool _)    = True
exp_const (String _)  = True
exp_const  _          = False


-- production #9
funF (a:b)                 = if (exp_const a) then Return b  -- const_exp
                                              else fX (a:b)  -- var or exp


-- production #9 (aux)
fX ((Id _):b)              = funY b  -- var Y => p#10
fX ((Symbol LPAREN):b)     = do
                              x <- funExpA b  -- parse expression => p#5
                              rec_rp x        -- recognize right bracket
fX (a:_)                   = Raise ("ERRORE in fX, TROVATO" ++ show(a))


-- production #10
funY ((Symbol LPAREN):b)      =  do
                                 x <- seq_exp b  -- parse expressions sequence
                                 rec_rp x        -- recognize right bracket
funY x                        = Return x


-- production #14
seq_exp :: [Token] -> Exc [Token]
seq_exp a = Raise[] -- da completare ......................................


-- production #15
seq_var :: [Token] -> Exc [Token]
seq_var a = Raise[] -- da completare ......................................
