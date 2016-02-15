-- LKC Compiler --> SECD
module Compilatore (
  comp_one,
  Secdexpr(..)
  ) where

import Analizzatore_sint_2
import Lexer

-- Datatype for possible SECD machine instructions: for more details, see
-- interpreter.hs
data Secdexpr = Add | Sub |  Mult | Div | Rem | Eq | Leq |
        Car | Cdr | Cons | Atom | Join | Rtn | Stop | Push |
        Ap | Rap | Ld (Integer, Integer) |
        Ldc LKC|
        Sel [Secdexpr] [Secdexpr]|
        Ldf [Secdexpr]
        deriving(Show, Eq)


-- Given the name of a variable, this function calculates its address in a
-- list of LKCs
position :: String -> [LKC] -> Integer
position x [] = error "position: can't find variable"
position x ((VAR z):y) = if z == x then 0 else 1 + (position x y)
position x _ = error "position: trovata non VAR"

-- This function returns iff the given string is a name of a variable in the
-- list of LKCs passed as second parameters
member:: String -> [LKC] -> Bool
member x []          = False 
member x ((VAR z):y) = if x == z then True else member x y
member x _           = error ("member: trovata non VAR"++ x)

-- Finds the relative position (control link, offset) of a variable in the
-- current static environment (passed as third parameter)
location :: String -> Integer -> [[LKC]]-> (Integer, Integer)
location x _ [] = error ("location non trova VAR "++ x)
location x ct (n:m) =
    if (member x n)
    then
      (ct, (position x n))
    else
      (location x (ct+1) m)


-- This function generates the SECD instructions that put on the stack the list
-- of the actual parameters, which are listed in the first parameter.
-- Example:
-- [(LKC, LKC)] = [(VAR "a",NUM a'),(VAR "b",NUM b')]
-- Output:
-- [Ldc NIL, Ldc (NUM 3), Cons, Ldc (NUM 2), Cons]
--          ^ b's index: 1      ^ a's index: 0
complist :: [LKC] -> [[LKC]] -> [Secdexpr]-> [Secdexpr]
complist [] _ c    = ((Ldc NIL):c)
complist (x:y) n c = complist y n (comp x n (Cons:c))


{- Compiler from LKC to SECD program.
 - It takes three arguments:
   * E: input LKC program
   * N: static environment (i.e. variables lists / activation records)
   * C: translation in SECD instructions that has been realized until the
        current invocation
 - Thus, 'comp' is able to give meaning to the LKC sequence we have been able
   to produce with the parser.
-}
comp :: LKC -> [[LKC]] -> [Secdexpr] -> [Secdexpr]
comp e n c =  case e of
    -- VAR => to load variable v on the top of the stack, we have to compute
    --        its address
    (VAR v)       -> ((Ld (location v 0 n)):c)
    -- NUM => load a numeric constant
    (NUM n)       -> (Ldc (NUM n)):c
    -- BOO => load a boolean constant
    (BOO b)       -> (Ldc (BOO b)):c
    -- BOO => load a string constant
    (STRI s)      -> (Ldc (STRI s)):c
    -- BOO => load an empty list
    (NIL)         -> (Ldc NIL):c
    -- ADD => compile an addition:
    -- * push 'Add' SECD expression
    -- * translate first term to an SECD expression
    -- * translate second term to an SECD expression
    (ADD f s)     -> comp s n (comp f n (Add:c))
    -- SUB => compile a subtraction:
    -- * push 'Sub' SECD expression
    -- * translate first term to an SECD expression
    -- * translate second term to an SECD expression
    (SUB f s)     -> comp s n (comp f n (Sub:c))
    -- MULT => compile a multiplication:
    -- * push 'Sub' SECD expression
    -- * translate first factor to an SECD expression
    -- * translate second factor to an SECD expression
    (MULT a b)    -> comp b n (comp a n (Mult:c))
    -- DIV => compile a division:
    -- * push 'Div' SECD expression
    -- * translate dividend to an SECD expression
    -- * translate divisor to an SECD expression
    (DIV f s)     -> comp s n (comp f n (Div:c))
    -- REM => compile a remainder operation:
    -- * push 'Div' SECD expression
    -- * translate dividend to an SECD expression
    -- * translate divisor to an SECD expression
    (REM f s)     -> comp s n (comp f n (Rem:c))
    -- EQC => compile a comparison:
    -- * push 'Eq' SECD expression
    -- * translate first expression to an SECD expression
    -- * translate second expression to an SECD expression
    (EQC f s)     -> comp s n (comp f n (Eq:c))
    -- LEQC => compile a less-or-equal comparison:
    -- * push 'Leq' SECD expression
    -- * translate first expression to an SECD expression
    -- * translate second expression to an SECD expression
    (LEQC f s)    -> comp s n (comp f n (Leq:c))
    -- CARC => compile a "head" operation:
    -- * push 'Car' SECD expression
    -- * translate list to an SECD expression
    (CARC h)      -> comp h n (Car:c)
    -- CDRC => compile a "tail" operation:
    -- * push 'Cdr' SECD expression
    -- * translate list to an SECD expression
    (CDRC t)      -> comp t n (Cdr:c)
    -- CONSC => compile a "conjunction" operation:
    -- * push 'Cons' SECD expression
    -- * translate element to be appended to an SECD expression
    -- * translate list to an SECD expression
    (CONSC f s)   -> comp s n (comp f n (Cons:c))
    -- ATOMC => compile an "atom" test:
    -- * push 'Atom' SECD expression
    -- * translate evaluated value to an SECD expression
    (ATOMC x)     -> comp x n (Atom:c)
    -- IFC => compile a conditional statement:
    -- * compile both branches (true_branch and false_branch)
    -- * pass true_branch and false_branch to 'Sel'
    -- * translate condition to an SECD expression
    (IFC a t f)   -> let
                       thenp = (comp t n [Join])
                       elsep = (comp f n [Join])
                     in
                       comp a n ((Sel thenp elsep):c)
    -- LAMBDAC => compile a function declaration:
    -- * put 'Rtn' at the end of the SECD instruction sequence that will be
    --     executed by the SECD machine as the function gets invoked
    -- * put the list of binders in front of the static environment N to get N'
    -- * compile the body of the function with static environment N' to get C'
    -- * wrap C' with an 'Ldf' constructor, so the closure will be put on the
    --     top of the stack by the interpreter
    (LAMBDAC l b) -> (Ldf (comp b (l:n) [Rtn])):c
    -- CALL => compile a function invocation:
    -- * put 'Ap' at the end of the SECD expressions sequence to make the SECD
    --     machine invoke the function
    -- * load the function on the top of the stack of SECD machine
    -- * compile the instructions to load the input parameters
    (CALL f l)    -> complist l n (comp f n (Ap:c))
    -- LETC => declare variables and then invoke a function:
    -- * separate L-values from R-values
    -- * combine the operations of function declaration (LAMBDAC) and function
    --     invocation (CALL)
    (LETC b l)    -> let
                       (vars, vals) = varsAndVals l
                       letBody = (Ldf (comp b (vars:n) [Rtn])):(Ap):c
                     in
                       -- compile a call to an (anonymous) function using:
                       -- * vals as actual parameters
                       -- * vars as formal parameters
                       complist vals n (letBody)
    -- LETRECC => declare variables and then invoke a function:
    -- * separate L-values from R-values
    -- * combine the operations of function declaration (LAMBDAC) and function
    --     invocation (CALL)
    -- * take in account the possibility to have recursion in this situation by
    --     putting 'Push' as first instruction of the compiled SECD code
    (LETRECC b l) -> let
                       (vars, vals) = varsAndVals l
                       letRecBody = (Ldf (comp b (vars:n) [Rtn])):(Rap):c
                     in
                       Push:(complist (vals) (vars:n) letRecBody)
    _ -> [];

-- Functions that transforms a list of pairs into a pair of lists
varsAndVals :: [(LKC, LKC)] -> ([LKC], [LKC])
varsAndVals ((variable, value):xs) = let
                                    (varList, valList) = varsAndVals xs
                                in
                                    (variable:varList, value:valList)
varsAndVals [] = ([],[])

--esempi di prova

--c="letrec  FACT = lambda ( X ) if  eq ( X, 0 ) then 1 else  X*FACT(  X - 1 ) and G = lambda ( H L ) if  eq ( nil, L ) then L else c--ons( H(car( L ) ), G ( H, cdr ( L ) )) in G ( FACT, cons(1 ,cons(2, cons(3, nil))) ) end $"



d = "let x= 5 and y= 6 in x*3 + y * 2* x + x*y end $"

comp_one x = comp x [] []
