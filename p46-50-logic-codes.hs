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

-- (*) 47. Truth tables for logical expressions (2).
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

