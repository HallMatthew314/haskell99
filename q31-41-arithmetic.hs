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
myGCD m 0 = m
myGCD m n = myGCD n $ if m < n then m else mod m n

-- 33. (*) Determine if two positive integer numbers are coprime.
-- Two numbers are coprime if their greatest common divisor equals 1.
coprime :: Integral a => a -> a -> Bool
coprime x y = 1 == myGCD x y

-- 34. (**) Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as
-- the number of positive integers (1 <= r < m) that are coprime to m.
-- Example:
-- m = 10
-- r = 1,3,7,9
-- thus phi(m) = 4
-- Note the special case: phi(1) = 1
totient :: Integral a => a -> Int
totient 1 = 1
totient m = length $ filter (coprime m) [1 .. (m - 1)]

