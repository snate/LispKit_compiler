module Lexer (
Token(..),
Symbol_T(..),
Operator_T(..),
Keyword_T(..),
lexi
) where

import Prelude hiding (EQ)


-- Types

-- Keywords of LISPKIT
data Keyword_T = LET | IN | END | LETREC | AND | IF | THEN | ELSE | LAMBDA
    deriving (Show,Eq)

-- Language functions
data Operator_T = EQ | LEQ | CAR | CDR | CONS | ATOM
    deriving (Show,Eq)

-- Algebraic (and miscellaneous) operators
data Symbol_T = LPAREN | RPAREN | EQUALS | PLUS | MINUS | TIMES | DIVISION |VIRGOLA | DOLLAR
    deriving (Show,Eq)

-- Types that a word in the input string can be
data Token = Keyword Keyword_T | Operator Operator_T | Id String |
    Symbol Symbol_T | Number Integer | String String | Bool Bool | Nil
    deriving (Show,Eq)



-- Auxiliary functions

-- True iff the given parameter is a letter character
isAlphaChar c = c `elem` (['a' .. 'z'] ++ ['A' .. 'Z'])

-- True iff the given parameter is a digit character
isDigitChar c = c `elem` ['0' .. '9']

-- True iff the given parameter is a letter or a digit
isIdChar c = isAlphaChar c || isDigitChar c

-- True iff the given parameter is a symbol between expressions
isSeparator c = c `elem` "()=$,"

-- True iff the given parameter is a separator of tokens
isSpace c = c `elem` [' ', '\n', '\f', '\r', '\t']

-- True iff the given parameter is a valid symbol in LispKit
isSymbol c = c `elem` "()=+-*/,"


{- Given a string X, compare it with LispKit's keywords and operators.
 - If it is one of them, returns a Token with the corresponding lexeme.
 - Otherwise, X is considered as an identifier (i.e. the name of a variable).
 -}
extractWord :: String -> Token
extractWord w = case w of
    "let"     -> Keyword LET
    "in"      -> Keyword IN
    "end"     -> Keyword END
    "letrec"  -> Keyword LETREC
    "and"     -> Keyword AND
    "if"      -> Keyword IF
    "then"    -> Keyword THEN
    "else"    -> Keyword ELSE
    "lambda"  -> Keyword LAMBDA

    "eq"      -> Operator EQ
    "leq"     -> Operator LEQ
    "car"     -> Operator CAR
    "cdr"     -> Operator CDR
    "cons"    -> Operator CONS
    "atom"    -> Operator ATOM

    "true"    -> Bool True
    "false"   -> Bool False

    "nil"     -> Nil
    
    otherwise -> Id w

-- Given a symbol character, returns the corresponding token
toSymbol :: Char -> Symbol_T
toSymbol c = case c of
    '(' -> LPAREN
    ')' -> RPAREN
    '+' -> PLUS
    '-' -> MINUS
    '*' -> TIMES
    '/' -> DIVISION
    '=' -> EQUALS
    ',' -> VIRGOLA



{-
 - These functions represents the possible automata states. Nota Bene: there
 - is no recursion.
 - The transition from initial state I to another state is realized as an
 - invocation.
 - The following transition will make the automata return to I.
 -}

-- N: automata state in which numbers are read
{- n input number sign
 - input: input string
 - number: number that has been read up to now
 - sign: true iff negative (sign is determined in state I)
 -}
n :: String -> Integer -> Bool -> (Token, String)
n "" _ _ = error "Unexpected end of string"
n input@(c:l) num sign
    | isDigitChar c =
        let d = read [c] :: Integer
        in n l (num*10 + d) sign
    | otherwise = (Number((if sign then -1 else 1) * num), input)

-- SC: automata state in which quoted strings are read
{- sc input res
 - input: input string
 - res: string that has been read up to now
 -}
sc :: String -> String -> (Token, String)
sc "" _ = error "Unexpected end of string"
sc ('"':l) res = (String res, l)
sc (c:l) res = sc l (res ++ [c])

-- S: automata state in which input could be identifiers, prefix operators or
--    keywords
{- s input res
 - input: input string
 - res: identifier that has been read up to now
 -}
s :: String -> String -> (Token, String)
s "" _ = error "Unexpected end of string"
s input@(c:l) res
    | isIdChar c = s l (res ++ [c])
    | otherwise = (extractWord(res), input)


-- I: main automata state.
--    Selects other input interpretation state depending on the next symbol on
--    the input 'tape'.
i :: String -> [Token]
i "" = error "Unexpected end of string in init"
i "$" = [(Symbol DOLLAR)]
i input@(f:l)
    | isSpace f = i l
    | f == '"' =                -- QUOTES THAT START A STRING
        let (tkn, str) = sc l ""
        in tkn : (i str)
    | isSymbol f =              -- SYMBOL (BRACKETS, EQUAL, PLUS, MINUS)
        ((Symbol $ toSymbol f) : (i l))
    | f == '~' =                -- NEGATIVE NUMBER
        let (tkn, str) = n l 0 True
        in (tkn : (i $ dropWhile (isDigitChar) l))
    | isDigitChar f =           -- POSITIVE NUMBER
        let (tkn, str) = n input 0 False
        in (tkn : (i $ dropWhile (isDigitChar) l))
    | otherwise =               -- KEYWORD, OPERATOR, BOOLEAN, NULL OR VARIABLE
        let (tkn, str) = s input ""
        in (tkn : (i str))


-- Main function and function exposed in module interface that executes the
--  lexical analysis of the input.
lexi :: String -> [Token]
lexi = i
