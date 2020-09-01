-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
-- https://wiki.haskell.org/99_questions/1_to_10
-- https://wiki.haskell.org/99_questions/11_to_20
-- https://wiki.haskell.org/99_questions/21_to_28

-- 1. (*) Find the last element of a list.
myLast :: [a] -> a
myLast [x]    = x
myLast (x:xs) = myLast xs

-- 2. (*) Find the last but one element of a list.
-- (second-last)
myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (x:xs) = myButLast xs

-- 3. (*) Find the K'th element of a list.
-- The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt xs 1 = head xs
elementAt xs n = elementAt (tail xs) (n - 1)

-- 4. (*) Find the number of elements of a list.
myLength :: [a] -> Int
myLength []     = 0
myLength [_]    = 1
myLength (x:xs) = 1 + myLength xs

-- 5. (*) Reverse a list.
myReverse :: [a] -> [a]
myReverse []     = []
myReverse [x]    = [x]
myReverse [x, y] = [y, x]
myReverse xs     = last xs:myReverse (init xs)

-- 6. (*) Find out whether a list is a palindrome.
-- A palindrome can be read forward or backward; e.g. (x a m a x). 
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs
    | length xs < 2      = True
    | head xs /= last xs = False
    | otherwise          = isPalindrome $ init $ tail xs

-- 7.(**) Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into
-- a `flat' list by replacing each list with its elements (recursively).

-- Originally stolen from wiki because I don't understand this data type.
-- Figured it out and re-implemented independently.
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List [])     = []
flatten (Elem x)      = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- 8. (**) Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single
-- copy of the element. The order of the elements should not be changed.
compress :: (Eq a) => [a] -> [a]
compress []     = []
compress [x]    = [x]
compress (x:xs) = if x == head xs
                    then compress $ x : tail xs
                    else x : compress xs

-- 9. (**) Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements
-- they should be placed in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
pack []     = []
pack [x]    = [[x]]
pack xs = packHelp $ map (\i -> [i]) xs

packHelp :: (Eq a) => [[a]] -> [[a]]
packHelp [x]      = [x] 
packHelp (x:y:xs) = if head x == head y
                    then packHelp ((head y : x) : xs)
                    else x:(packHelp $ y:xs)

-- 10. (*) Run-length encoding of a list.
-- Use the result of problem P09 to implement the so-called
-- run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists/tuples (N E)
-- where N is the number of duplicates of the element E.
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\t -> (length t, head t)) (pack xs)

-- 11. (*) Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element
-- has no duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists.
data MultiSingle a = Single a
                   | Multiple Int a
                   deriving (Show)

msConv :: (Int, a) -> MultiSingle a
msConv t = if fst t == 1
           then Single $ snd t
           else Multiple (fst t) (snd t)

encodeModified :: (Eq a) => [a] -> [MultiSingle a]
encodeModified xs = map msConv (encode xs)

-- 12. (**) Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11,
-- construct its uncompressed version.
decodeModified :: [MultiSingle a] -> [a]
decodeModified []                  = []
decodeModified ((Single x):xs)     = x : decodeModified xs
decodeModified ((Multiple c x):xs) = (take c $ repeat x) ++ decodeModified xs

-- 13. (**) Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression
-- method directly. i.e. don't explicitly create the sublists
-- containing the duplicates, as in problem 9, but only count them.
-- As in problem P11, simplify the result list by replacing
-- the singleton lists (1 X) by X.
encodeDirect :: (Eq a) => [a] -> [MultiSingle a]
encodeDirect [] = []
encodeDirect xs = edHelp $ map (\x -> Single x) xs

edHelp :: (Eq a) => [MultiSingle a] -> [MultiSingle a]
edHelp []                         = []
edHelp [x]                        = [x]
edHelp [Single x, Single y]       = if x == y
                                    then [Multiple 2 x]
                                    else [Single x, Single y]
edHelp [Multiple c x, Single y]   = if x == y
                                    then [Multiple (c + 1) x]
                                    else [Multiple c x, Single y]
edHelp (Single x:Single y:xs)     = if x == y
                                    then edHelp (Multiple 2 x:xs)
                                    else Single x:edHelp (Single y:xs)
edHelp (Multiple c x:Single y:xs) = if x == y
                                    then edHelp (Multiple (c + 1) x:xs)
                                    else Multiple c x:edHelp (Single y:xs)

-- 14. (*) Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x:x:dupli xs

-- 15. (**) Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli [] _     = []
repli (x:xs) c = (take c $ repeat x) ++ repli xs c

-- 16. (**) Drop every N'th element from a list. (first is 1, not 0)
dropEvery :: [a] -> Int -> [a]
dropEvery xs c = map fst [t | t <- z, snd t `mod` c /= 0]
                where z = zip xs [1 .. ]

-- 17. (*) Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates. 
split :: [a] -> Int -> ([a], [a])
split xs 0     = ([], xs)
split (x:xs) c = (x:fst t, snd t)
               where t = split xs (c - 1)

-- 18. (**) Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing
-- the elements between the i'th and k'th element of the original list
-- (both limits included).
-- Start counting the elements with 1.
slice :: [a] -> Int -> Int -> [a]
slice _ 0 _  = undefined
slice xs 1 k = take k xs
slice xs i k = slice (tail xs) (i - 1) (k - 1)

-- 19. (**) Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate [] _     = []
rotate [x] _    = [x]
rotate xs i
    | i > 0     = rotate (tail xs ++ [head xs]) (i - 1)
    | i < 0     = rotate (last xs:init xs) (i + 1)
    | otherwise = xs

-- 20. (*) Remove the K'th element from a list. (1-indexed)
removeAt :: Int -> [a] -> (a, [a])
removeAt 1 xs     = (head xs, tail xs)
removeAt c (x:xs) = (fst t, x:snd t)
                  where t = removeAt (c - 1) xs

-- 21. Insert an element at a given position into a list. (1-indexed)
insertAt :: a -> [a] -> Int -> [a]
insertAt _ _ 0  = undefined
insertAt x [] _ = [x]
insertAt x xs 1 = x:xs
insertAt x xs i = head xs:insertAt x (tail xs) (i - 1)

-- 22. Create a list containing all integers within a given range. (inclusive)
range :: Int -> Int -> [Int]
range x y
    | x > y     = []
    | x == y    = [x]
    | otherwise = x:range (x + 1) y
