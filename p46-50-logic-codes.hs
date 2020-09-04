-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
-- https://wiki.haskell.org/99_questions/46_to_50

import Text.Printf

-- 46. (**) Define predicates for these logical/boolean operations:
-- AND, OR, NAND, NOR, XOR, IMPL, EQU
-- A logical expression with two variables can then be written like:
-- `and' (or' a b) (nand' a b)`
-- After this, write a function `table` which prints
-- the truth table of a given logical expression.
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _       = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _         = True

nand' :: Bool -> Bool -> Bool
nand' True True = False
nand' _ _       = True

nor' :: Bool -> Bool -> Bool
nor' False False = True
nor' _ _         = False

xor' :: Bool -> Bool -> Bool
xor' True True   = False
xor' False False = False
xor' _ _         = True

impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _        = True

equ' :: Bool -> Bool -> Bool
equ' True True   = True
equ' False False = True
equ' _ _         = False

tableLine :: (Bool -> Bool -> Bool) -> (Bool,Bool) -> String
tableLine f t =
    let a = show $ fst t
        b = show $ snd t
        c = show $ f (fst t) (snd t)
    in  printf "%s %s %s\n" a b c

table :: PrintfType r => (Bool -> Bool -> Bool) -> r
table f =
    let a = tableLine (f) (True,True)
        b = tableLine (f) (True,False)
        c = tableLine (f) (False,True)
        d = tableLine (f) (False,False)
    in  printf "%s%s%s%s" a b c d

-- 47. (*) Truth tables for logical expressions (2).
-- Continue problem P46 by defining and/2, or/2, etc as being operators.
-- This allows to write the logical expression in the more natural way,
-- as in the example: A and (A or not B). Define operator precedence
-- as usual; i.e. as in Java.

-- Precedence: not, eq, and, xor, or, impl
infixl 4 `equ'`
infixl 3 `and'`
infixl 3 `nand'`
infixl 2 `xor'`
infixl 1 `or'`
infixl 1 `nor'`
infixl 0 `impl'`

-- 48. (**) Truth tables for logical expressions (3).
-- Generalize problem 47 in such a way that the logical expression may
-- contain any number of logical variables. Define tablen in a way that
-- (tablen list expr) prints the truth table for the expression (expr),
-- which contains the logical variables enumerated in (list).
genBoolArgs :: Int -> [[Bool]]
genBoolArgs 0 = []
genBoolArgs 1 = [[True],[False]]
genBoolArgs n =
    let prev  = genBoolArgs $ n - 1
        tHalf = map (\bs -> True:bs) prev
        fHalf = map (\bs -> False:bs) prev
    in  tHalf ++ fHalf

btos :: String -> Bool -> String
btos ""  b = show b
btos del b = show b ++ del

tablenLine :: ([Bool] -> Bool) -> [Bool] -> String
tablenLine f args = foldl (++) "" $ map (btos " ") args ++ [btos "" (f args)]

tablen :: PrintfType r => Int -> ([Bool] -> Bool) -> r
tablen n f =
    let args = genBoolArgs n
    in  printf $ unlines $ map (tablenLine f) args

-- 49. (**) Gray codes.

-- An n-bit Gray code is a sequence of n-bit strings constructed
-- according to certain rules. For example,
-- n = 1: C(1) = ['0','1'].
-- n = 2: C(2) = ['00','01','11','10'].
-- n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
-- Find out the construction rules and write a predicate
-- with the following specification:
-- % gray(N,C) :- C is the N-bit Gray code
gray :: Int -> [String]
gray 1 = ["0","1"]
gray n = map (\s -> '0':s) prev ++ map (\s -> '1':s) (reverse prev)
        where prev = gray (n - 1)

