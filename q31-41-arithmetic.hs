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

-- 35. (**) Determine the prime factors for a given positive integer.
-- Construct a flat list containing the prime factors in ascending order.
primeFactors :: Int -> [Int]
primeFactors n
    | n < 2     = []
    | otherwise = p:primeFactors (div n p)
    where p = head [x | x <- primes, mod n x == 0]

sieve :: [Int] -> [Int]
sieve (p:ps) = p : sieve [x | x <- ps, mod x p /= 0]

primes :: [Int]
primes = sieve [2 .. ]

-- 36. (**) Determine the prime factors for a given positive integer.
-- Construct a list containing the prime factors and their multiplicity.
-- Example: 315 -> [(3,2),(5,1),(7,1)]
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = pack . primeFactors

pack :: (Eq a) => [a] -> [(a, Int)]
pack []  = []
pack [x] = [(x, 1)]
pack xs  = packHelp $ map (\i -> (i, 1)) xs

packHelp :: (Eq a) => [(a, Int)] -> [(a, Int)]
packHelp [x]      = [x]
packHelp (x:y:xs) = if fst x == fst y
                    then packHelp ((fst x, snd x + 1):xs)
                    else x:packHelp (y:xs)

-- 37. (**) Calculate Euler's totient function phi(m) (improved).
-- If the list of prime factors of a number m is known in the form
-- of problem 36 then the function phi(m) can be efficiently
-- calculated as follows:
-- Let ( (p1 m1) (p2 m2) (p3 m3) ... ) be the list of prime factors
-- (and their multiplicities) of a given number m. phi(m) can then be
-- calculated with the following formula:
-- phi(m) = (p1 - 1) * p1^(m1 - 1) *
--          (p2 - 1) * p2^(m2 - 1) *
--          (p3 - 1) * p3^(m3 - 1) * ...
totientFast :: Int -> Int
totientFast n = product [(fst t - 1) * (fst t)^(snd t - 1) | t <- fs]
                where fs = primeFactorsMult n

-- 38. (*) Compare the two methods of calculating Euler's totient function.
-- Use the solutions of problems 34 and 37 to compare the algorithms.
-- Take the number of reductions as a measure for efficiency.
-- Try to calculate phi(10090) as an example.

-- Observations: 37 is faster than 34

-- 39. (*) A list of prime numbers.
-- Given a range of integers by its lower and upper limit,
-- construct a list of all prime numbers in that range.
primesR :: Int -> Int -> [Int]
primesR s e = [x | x <- takeWhile (<e) primes, x >= s]

-- 40. (**) Goldbach's conjecture.
-- Goldbach's conjecture states that every positive even number greater
-- than 2 is the sum of two prime numbers (28 = 5 + 23). Write a function
-- to find the two prime numbers that sum up to a given even integer.
goldbach :: Int -> (Int,Int)
goldbach n
    | mod n 2 == 1 = undefined
    | n == 2       = undefined
    | n == 4       = (2,2)
    | otherwise    = head [(p, n - p) | p <- ps, isPrime (n - p)]
    where ps = tail $ takeWhile (<n) primes

-- 41. (**) Given a range of integers by its lower and upper limit,
-- get a list of all even numbers and their goldbach composition.
goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList s e = [goldbach n | n <- [s .. e], n > 2 && mod n 2 == 0]

-- In most cases, if an even number is written as
-- the sum of two prime numbers, on of them is very small.
-- Very rarely, the primes are both bigger than 50. Try to
-- find out how many such cases there are in the range 2 .. 2000.
goldbachList' :: Int -> Int -> Int -> [(Int,Int)]
goldbachList' s e b = filter (pred) $ goldbachList s e
                    where pred = (\t -> fst t > b && snd t > b)

-- >goldbachList' 4 2000 50

