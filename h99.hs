-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

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
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

-- Example:
-- λ> flatten (Elem 5)
-- [5]
-- λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- λ> flatten (List [])
-- []

data NestedList a = Elem a | List [NestedList a]

-- TODO: actually solve this one
-- stolen from wiki because I don't understand this data type
flatten :: NestedList a -> [a]
flatten (List [])     = []
flatten (Elem x)      = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

