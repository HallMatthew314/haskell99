-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
-- https://wiki.haskell.org/99_questions/31_to_41

-- 31. (**) Determine whether a given integer is prime.
isPrime :: Integral a => a -> Bool
isPrime k
    | k < 2        = False
    | k == 2       = True
    | mod k 2 == 0 = False
    | otherwise = null [i | i <- r, mod k i == 0]
    where r = [3,5 .. floor $ sqrt $ fromIntegral k]

-- 32. (**) Determine the greatest common divisor
-- of two positive integers. Use Euclid's algorithm.
myGCD :: Integral a => a -> a -> a
myGCD x y = undefined

