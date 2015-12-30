--Compilatore LKC --> SECD incompleto Dicembre  2015
module Compilatore (
  comp_one,
  Secdexpr(..)
  ) where

import Analizzatore_sint_2
import Lexer



data Secdexpr = Add | Sub |  Mult | Div | Rem | Eq | Leq |
        Car | Cdr | Cons | Atom | Join | Rtn | Stop | Push |
        Ap | Rap | Ld (Integer, Integer) |
        Ldc LKC|
        Sel [Secdexpr] [Secdexpr]|
        Ldf [Secdexpr]
        deriving(Show, Eq)


-- funzioni per il calcolo dell'indirizzo di una variabile nell'ambiente 
position::String -> [LKC] -> Integer
position x [] = error "position: non trova la variabile"
position x ((VAR z):y) = if z==x then 0 else 1 + (position x y)
position x _ = error "position: trovata non VAR"

member:: String -> [LKC] -> Bool
member x []          = False 
member x ((VAR z):y) = if x == z then True else member x y
member x _           = error ("member: trovata non VAR"++ x)

location:: String->Integer-> [[LKC]]-> (Integer, Integer)
location x _ []= error ("location non trova VAR "++ x)
location x ct (n:m) =
    if (member x n)
    then
      (ct, (position x n))
    else
      (location x (ct+1) m)


sexpr_reverse:: [a] -> [a]
sexpr_reverse [] = []
sexpr_reverse (a:b)= (sexpr_reverse b) ++ [a]


--togliere variabili / espressioni da una lista di Binders
vars:: [(a,b)] -> [a]
vars [] = []
vars ((x,y):r) = (x : (vars r))

exprs:: [(a,b)] -> [b]
exprs [] = []
exprs((x,y):r) = (y:(exprs r))

complist:: [LKC] -> [[LKC]] -> [Secdexpr]-> [Secdexpr]
complist [] _ c    = ((Ldc NIL):c)
complist (x:y) n c = complist y n (comp x n (Cons:c))


comp:: LKC -> [[LKC]] -> [Secdexpr] -> [Secdexpr]
comp e n c =  case e of
    (VAR v)       -> ((Ld (location v 0 n)):c)
    (NUM n)       -> (Ldc (NUM n)):c
    (BOO b)       -> (Ldc (BOO b)):c
    (STRI s)      -> (Ldc (STRI s)):c
    (NIL)         -> (Ldc NIL):c 
    (ADD f s)     -> comp s n (comp f n (Add:c))
    (SUB f s)     -> comp s n (comp f n (Sub:c))
    (MULT a b)    -> comp b n (comp a n (Mult:c))
    (DIV f s)     -> comp s n (comp f n (Div:c))
    (REM f s)     -> comp s n (comp f n (Rem:c))
    (EQC f s)     -> comp s n (comp f n (Eq:c))
    (LEQC f s)    -> comp s n (comp f n (Leq:c))
    (CARC h)      -> comp h n (Car:c)
    (CDRC t)      -> comp t n (Cdr:c)
    (CONSC f s)   -> comp s n (comp f n (Cons:c))
    (ATOMC x)     -> comp x n (Atom:c)
    (IFC a t f)   -> let
                       thenp = (comp t n [Join])
                       elsep = (comp f n [Join])
                     in
                       comp a n ((Sel thenp elsep):c)
    (LAMBDAC l b) -> (Ldf (comp b (l:n) [Rtn])):c
    (LETC b l)    -> let
                       (vars, vals) = varsAndVals l
                       letBody = (Ldf (comp b (vars:n) [Rtn])):(Ap):c
                     in
                       complist vals n (letBody)
                       -- compile a call to (anonymous) function body b using:
                       -- * vals as actual parameters
                       -- * vars as formal parameters
    (LETRECC x y) -> [] -- TODO
    (CALL f l)    -> complist l n (comp f n (Ap:c))
    _ -> [];

varsAndVals :: [(LKC, LKC)] -> ([LKC], [LKC])
varsAndVals ((variable,value):xs) = let
                                    (varList, valList) = varsAndVals xs
                                in
                                    (variable:varList, value:valList)
varsAndVals [] = ([],[])

--esempi di prova

--c="letrec  FACT = lambda ( X ) if  eq ( X, 0 ) then 1 else  X*FACT(  X - 1 ) and G = lambda ( H L ) if  eq ( nil, L ) then L else c--ons( H(car( L ) ), G ( H, cdr ( L ) )) in G ( FACT, cons(1 ,cons(2, cons(3, nil))) ) end $"



d= "let x= 5 and y= 6 in x*3 + y * 2* x + x*y end $"

comp_one x = comp x [] []
