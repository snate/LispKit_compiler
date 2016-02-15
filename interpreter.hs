-- COMPLETE SECD INTERPRETER (written in Haskell)
module Interprete (
  interprete,
  Valore(..),
  ) where

import Compilatore
import Analizzatore_sint_2
import Lexer

-- Datatype that represents the R-values of variables.
-- This values will be the ones that appear in the stack S and in the dynamic
-- environment E.
data Valore = V LKC |  -- wraps an LKC (instruction set) value
              OGA |  -- OGA = placeholder for recursive environment
              CLO [Secdexpr] [[Valore]] |  -- closure constructor
              VLISTA [Valore]  -- list of values
              deriving(Show,Eq)

-- Datatype of Dump's possible values
data Dump = CONTR  [Secdexpr] |  -- constructor for if statement dump
            TRIPLA [Valore][[Valore]][Secdexpr] | -- constructor for SECD state
            DUMMY  -- placeholder for empty dump
            deriving(Show,Eq)


-- Function that creates the dynamic environment for a recursive function call.
lazyE :: [Valore] -> [Valore] -> [Valore]
lazyE [] _ = []
lazyE (a:b) c = ((lazyClo a c):(lazyE b c))

-- Puts the appropriate value for each parameter passed in a VLISTA argument
-- of a recursive apply.
lazyClo :: Valore -> [Valore] -> Valore
lazyClo (CLO a b) c  = (CLO a ((lazyE c c):(tail b)))
lazyClo (V x) _      = (V x)
lazyClo (VLISTA x) _ = (VLISTA x)
lazyClo x _= error ("LazyClo trova valore incompatibile" ++ (show x))


{- Function that takes a list and an offset, returning the n-th element of
   that list.
 - @param n: index of element that has to be returned
 - @param l: list
 -}
index :: Integer -> [a] -> a
index n l = if n == 0 then (head l) else (index (n-1) (tail l))

{- Function that takes a couple of Integers (a,b) as input in order to fetch a
   value from a list of lists.
 - @param a: search the value in the a-th list (starting from the last one)
 - @param b: index of the value that has to be fetched from the a-th list
 -}
locate :: (Integer, Integer) -> [[a]] -> a
locate  (a,b) e = (index b (index a e));

-- Function that removes the context from a boxed Integer value
extract_int :: Valore -> Integer
extract_int (V (NUM x)) = x
extract_int x = error ("trovato altro da intero" ++ (show x))

-- Function that returns the head of a VLISTA
vhd :: Valore -> Valore
vhd (VLISTA (a:b)) = a
vhd (VLISTA [])  = error "vhd trovata lista vuota"
vhd _ = error "vhd non trova VLISTA"

-- Function that returns the tail of a VLISTA
vtl :: Valore -> Valore
vtl (VLISTA (a:b)) = VLISTA b
vtl (VLISTA [])  = error "vtl trovata lista vuota";
vtl _ = error "vtl non trova VLISTA"

-- Function that returns True iff the given argument is an atom
vatom :: Valore -> Valore
vatom (V k)= V (BOO True)
vatom _ = V (BOO False)

-- evaluate b and return a corresponding "boolean" LKC
bool2s_expression:: Bool -> LKC
bool2s_expression b = if b then (BOO True) else (BOO False)

-- compares two values (returns true iff they are equal)
eqValore :: Valore -> Valore -> Bool
eqValore a@(V _) b = (eqV a b)
eqValore a@(VLISTA _) b = (eqVLISTA a b)
eqValore a  b = error ("uguaglianza tra chiusure"++ (show a) ++ (show b))

-- compares two VLISTAs (returns true iff they are equal)
eqVLISTA :: Valore -> Valore -> Bool
eqVLISTA (VLISTA []) (VLISTA [])= True
eqVLISTA (VLISTA(a:b)) (VLISTA (c:d)) = (eqValore a c) &&
                                        (eqVLISTA (VLISTA b) (VLISTA d))
eqVLISTA _ _= False

-- compares two Vs (returns true iff they are equal), i.e. two LKCs
eqV (V a) (V b) = a == b
eqV _ _ = False


{- interprete works as the virtual machine SECD, where:
 - @param S(tack): list of values which facilitates the use of polish notation
 - @param E(nvironment): *dynamic* environment, which is made of a list of
        records (or frames, if we consider an analogy with the JVM).
        It is invariant that every instruction that executes on the SECD
        machine can find the values it needs in E, without using Access Links
        or Heap, since a copy(¹) of E is put in every closure's right half
        (¹): only the variables name are copied so the SECD machine is able to
             determine the location of values at runtime
 - @param C(ontrol): list of remaining SECD instructions that have to be
        executed on SECD machine
 - @param D(ump): memory area where SECD snapshots are put before calling a
        function.
        The last Dump is restore on Rtn (returning from an invocation)
 -}
interprete:: [Valore] -> [[Valore]]-> [Secdexpr]-> [Dump]-> Valore
interprete s e c d = case (head c) of
    {- Loads a value on the top of the stack S using the address (b,n).
       * b: index of activation record
       * n: index of the variable in the b-th record
    -}
    Ld(b, n) -> let
                    x = (locate (b,n) e)
                in
                    (interprete (x:s) e (tail c) d)

    {- Loads a value k on the top of the stack S.
    -}
    (Ldc k) -> case k of
        NIL -> (interprete ((VLISTA []):s) e (tail c) d)
        _   -> (interprete ((V k):s) e (tail c) d)

    {- Performs an addition between two operands, which will be found on the
       top of stack S.
     - The result is put on the top of the stack, after removing the two
       operands.
    -}
    Add -> let
                operand1 = extract_int (head s)
                operand2 = extract_int(head (tail s))
           in
                (interprete ((V(NUM (operand1 + operand2))):(tail (tail s)))
                  e (tail c) d)

    {- Performs a difference between two operands, which will be found on the
       top of stack S.
     - The result is put on the top of the stack, after removing the two
       operands.
    -}
    Sub -> let
                operand1 = extract_int (head s)
                operand2 = extract_int(head (tail s))
           in
                (interprete ((V(NUM (operand1 - operand2))):(tail (tail s)))
                  e (tail c) d)

    {- Performs a multiplication between two operands, which will be found on
       the top of stack S.
     - The result is put on the top of the stack, after removing the two
       operands.
    -}
    Mult -> let
               operand1 = extract_int (head s)
               operand2 = extract_int(head (tail s))
            in
               (interprete ((V(NUM (operand1 * operand2))):(tail (tail s)))
                 e (tail c) d)

    {- Performs a division between two operands, which will be found on the
       top of stack S.
     - The result is put on the top of the stack, after removing the two
       operands.
    -}
    Div -> let
               operand1 = extract_int (head s)
               operand2 = extract_int(head (tail s))
           in
               (interprete ((V(NUM (operand1 `div` operand2))):(tail (tail s)))
                 e (tail c) d)

    {- Calculates the remainder of a division between two operands, which will
       be found on the top of stack S.
     - The result is put on the top of the stack, after removing the two
       operands.
    -}
    Rem -> let
               operand1 = extract_int (head s)
               operand2 = extract_int(head (tail s))
           in
               (interprete ((V(NUM (operand1 `mod` operand2))):(tail (tail s)))
                 e (tail c) d)

    {- Compares two numbers, which will be found on the top of stack S.
     - The result (T/F) is put on the top of the stack, after removing the two
       operands.
    -}
    Leq -> let
               operand1 = extract_int (head s)
               operand2 = extract_int(head (tail s))
           in
              (interprete ((V(bool2s_expression
                    (operand1 <= operand2))):(tail (tail s)))
                e (tail c) d)

    {- Compares two numbers, which will be found on the top of stack S.
     - The result (T/F) is put on the top of the stack, after removing the two
       operands.
    -}
    Eq -> case s of
        (w1:w2:w3) ->(interprete ((V (bool2s_expression (eqValore w1 w2))):w3)
            e (tail c) d)
        _-> error "manca un argomento in Eq"

    {- Takes the first element of a list, that will be found on the top of
       stack S.
     - The element is put on the top of the stack, after removing the list.
    -}
    Car -> (interprete ((vhd(head s)):(tail s)) e (tail c) d)

    {- Takes the tail of a list, that will be found on the top of stack S.
     - The tail is put on the top of the stack, after removing the list.
    -}
    Cdr -> (interprete ((vtl(head s)):(tail s)) e (tail c) d)

    {- Creates a list appending an element to an already existing list (those
       ones will be found on the top of stack S).
     - The resulting list is put on the top of the stack, after removing the
       elements that were joined.
    -}
    Cons -> case head (tail s) of
        (VLISTA x) -> (interprete (VLISTA ((head s):x):(tail (tail s)))
                        e (tail c) d)
        x-> error ("CONS: il secondo argomento non e' una lista" ++ (show  x))

    {- Returns true if the value on the top of the stack S is not a list.
     - The boolean value is put on the top of the stack, after removing the
       evaluated element.
    -}
    Atom -> (interprete ((vatom (head s)):(tail s)) e (tail c) d)

    {- Represents an if statement:
       * if on the top of the stack there is a 'True' value, executes the
         first branch;
       * if on the top of the stack there is a 'False' value, executes the
         second branch.
     - Regardless of the branch, the remainder of the program has to be saved
       on the dump.
    -}
    Sel sl1 sl2 -> case head s of
      (V (BOO True)) -> (interprete (tail s) e sl1 ((CONTR (tail c)):d))
      (V (BOO False)) -> (interprete (tail s) e sl2 ((CONTR (tail c)):d))
      _ -> error "non c'e' bool su s quando si esegue SEL"

    {- Resumes the execution of the SECD program after the if statement.
     - The following instructions can be found on the top of the dump D.
    -}
    Join -> case (head d) of
        (CONTR c1) -> (interprete s e c1 (tail d))
        _ -> error "JOIN: il dump non contiene controllo"

    {- Loads a function with code 'sl' on the top of the stack S, putting 'sl'
       as left part and the dynamic environment E as the right part.
    -}
    Ldf sl -> (interprete ((CLO sl e):s) e (tail c) d)

    {- Resumes the execution of the SECD program after a function invocation.
     - The result of the function can be found on the top of the stack S.
     - The following instructions can be found on the top of the dump D.
    -}
    Rtn -> case (head d) of
        (TRIPLA s1 e1 c1) -> (interprete ((head s):s1) e1 c1 (tail d))
        _ ->  error  "RTN: non trovata TRIPLA su dump"

    {- Applies the function that is on the top of the stack, after which there
       is the list of the actual parameters.
     - The list of the actual parameter will be the last activation record
       after the dynamic environment which was saved at the definition of the
       function.
     - There is the need to save the current SECD state (S,E,C) in the dump,
       so we can resume the current execution flow after the Rtn instruction in
       the function that we're applying.
    -}
    Ap -> case (head s) of
        (CLO c1 e1) -> case (head (tail s)) of
            VLISTA x -> (interprete [] (x:e1) c1 ((TRIPLA (tail(tail s))
                          e (tail c)):d))
            _  -> error "AP senza lista dei parametri"
        _  -> error "AP senza chiusura su s"

    {- Applies the recursive function that is on the top of the stack, after
       which there will be the list of the actual parameters.
     - We can make the same assumptions that we made for 'Ap', but the list of
       parameters has to be lazily evaluated.
    -}
    Rap -> case (head s) of
        (CLO c1 e1) ->  case e1 of
            ([OGA]:re) -> case (head (tail s)) of
                (VLISTA vl2) -> (interprete [] ((lazyE vl2 vl2):re) c1
                                ((TRIPLA (tail (tail s)) (tail e) (tail c)):d))
                _ -> error "manca [OGA] sull'ambiente di chiusura ric"
            _ -> error "non trovata [OGA] nell'ambiente di chiusura ricorsiva"
        _  -> error "RAP: non trovata chiusura su s"

    {- Pushes an OGA placeholder at the end of the dynamic environment E.
     - This placeholder is needed in order to determine the right offset for
       'Ld' operations while dealing with non-recursive binders in recursive
       application.
     - If [OGA] weren't put, we could have some of the Ld addresses wrong by 1.
    -}
    Push -> (interprete s ([OGA]:e) (tail c) d)

    {- Stops the SECD machine.
     - The value of the entire expression will be found on the top of the
       stack S.
    -}
    Stop -> (head s)

    {- Should be removed.
    -}
    _  -> error "operazione non riconosciuta"

-- syntactic sugar function
fin x = (interprete [] [] (x ++ [Stop]) [])  -- se x è il programma Secdexpr
                                             -- da eseguire. Si aggiunge Stop
                                             -- alla fine.



e = "let z=2 in letrec x= 2+z and y= 2*z in x*y*z end end $"
--ok

-- distribuisce FACT su una lista di interi *)

--val S = "letrec  FACT = lambda ( X ) if  eq ( X, 0 ) then 1 else  X * FACT(  X - 1 )"^
--" and G = lambda ( H  L ) if  eq ( nil, L ) then L else cons( H(car( L ) ), G ( H , cdr ( L ) ))"^
--" in G ( FACT, cons( 6 ,cons( 7, cons( 8 , nil))) ) end $";

-- (*considera liste di liste Z e produce una lista semplice che contiene
-- tanti interi quante sono le liste contenute in Z e l'intero
-- corrispondente ad una lista contenuta in Z è la somma dei fattoriali dei
-- suoi elementi: f2=fattoriale, f1=calcola somma dei fattoriali degli
-- elementi di una lista di interi e f0 distribuisce f1 sulle liste contenute
-- in Z *)

f = "letrec f0 = lambda ( x ) letrec f1 = lambda(y) letrec f2=lambda (z) if eq(z , 1) then 1 else z * f2( z - 1 ) in if eq( y , nil ) then 0 else f2 ( car ( y ) ) + f1 ( cdr (y)) end in if eq(x , nil) then nil else cons (f1 ( car ( x )),f0 ( cdr ( x ) ) ) end in f0( cons (cons (3 , cons (3 , nil)), cons( cons (3 , nil), nil))) end $"
-- ok


--(* esempio di funzione che restituisce una funzione locale *)

g="let f1 = lambda() letrec f2=lambda (z) if eq(z , 1) then 1 else z * f2( z - 1 ) in f2 end in let x=f1() in x(8) end end $"
--ok


--Tok=lexi(explode S)

--(k1,k2)=PROG(Tok)

a = "letrec fact = lambda(n) if eq(n, 1) then 1 else n*fact(n-1) in fact(5) end $"
j = "let z=2 in letrec x= 3+z in x+z end end $"
k = "letrec z=2 in let x= 3+z in x+z end end $"
q = "letrec z=2 in letrec x= 3+z in x+z end end $"
tpnt18 = "let x = lambda (a b) cons(a, b) in x end $";
tpnt19 = "let x= lambda (a) a in x ( car(cons(1, nil)) ) end $";
tpnt20 = "lets x= lambda (a) a in x ( car(cons(1, nil)) ) end $";
-- ^ (wrong case)
tpnt21 = "let a=2 and b = 3 in a end $"

b = lexi tpnt21
c = prog b
d = case c of
    Return parsed_program -> comp_one parsed_program
    Raise exc -> error ("Raised: " ++ exc)
h = fin d
