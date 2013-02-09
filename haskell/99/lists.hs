-- Problem 1
-- Find the last element of a list
myLast :: [a] -> a
myLast []     = error "No elements."
myLast [x]    = x
myLast (_:xs) = myLast xs


-- Problem 2
-- Find the last but one element of a list.
myButLast :: [a] -> a
myButLast = last . init


-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1.
-- "abc" 1 -> 'a'
-- elementAt :: [a] -> a
-- ??? :)

-- Problem 4
-- Find the number of elements of a list.
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- Problem 5
-- Reverse a list.
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = (myReverse xs) ++ [x]


-- Problem 6
-- Find out whether a list is a palindrome. A palindrome can be read forward or
-- backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [x, y]    = x == y
isPalindrome [x, _, z] = x == z
isPalindrome (x:xs)    = x == last xs && isPalindrome (init xs)


-- Problem 8
-- "aaaa" -> "a"
myCompress :: Eq a => [a] -> [a]
myCompress []     = []
myCompress (x:xs) = x : (myCompress $ dropWhile (== x) xs)


-- Problem 10
myEncode :: Eq a => [a] -> [(Int, a)]
myEncode []     = []
myEncode (x:xs) = (myLength (takeWhile (== x) xs) + 1, x) : (myEncode $ dropWhile (== x) xs)


data NestedList a = Elem a | List [NestedList a]

-- ??
myFlatten :: NestedList a -> [a]
myFlatten (Elem a) = [a]
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)
myFlatten (List []) = []


-- Problem 9
-- Pack consecutive duplicates of list elements into sublists. If a list
-- contains repeated elements they should be placed in separate sublists.
myPack :: [Char] -> [String]
myPack []     = []
myPack (x:xs) = [x : takeWhile (== x) xs] ++ myPack (dropWhile (== x) xs)


encode :: Eq a => [a] -> [(Int, a)]
encode []     = []
encode (x:xs) = (count, x) : encode (dropWhile (== x) xs)
              where count = 1 + length (takeWhile (== x) xs)


data EncodingEntry a = Single a | Multiple Int a
    deriving Show


toTuple :: EncodingEntry a -> (Int, a)
toTuple (Single a)     = (1, a)
toTuple (Multiple n a) = (n, a)


encodeElement :: Int -> a -> EncodingEntry a
encodeElement 1 x = Single x
encodeElement n x = Multiple n x


encodeModified :: Eq a => [a] -> [EncodingEntry a]
encodeModified []     = []
encodeModified (x:xs) = (encodeElement count x): encodeModified (dropWhile (== x) xs)
                      where count = 1 + length (takeWhile (== x) xs)


encodeDirect :: Eq a => [a] -> [EncodingEntry a]
encodeDirect []     = []
encodeDirect (x:xs) = encodeDirect' 1 x xs

encodeDirect' n y []                 = [encodeElement n y]
encodeDirect' n y (x:xs) | y == x    = encodeDirect' (n+1) y xs
                         | otherwise = encodeElement n y : (encodeDirect' 1 y xs)


decodeModified :: Eq a => [EncodingEntry a] -> [a]
decodeModified []     = []
decodeModified (x:xs) = (take count $ repeat $ snd $ toTuple x) ++ decodeModified xs
                      where count = fst $ toTuple x


dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = (x:x:(dupli xs))


repli :: [a] -> Int -> [a]
repli []     n = []
repli (x:xs) n = replicate n x ++ (repli xs n)

myDrop :: [a] -> Int -> [a]
myDrop [] n = []
myDrop xs n = myDrop' 1 xs n

myDrop' _     []     _                    = []
myDrop' count (x:xs) n | mod count n == 0 = rest
                       | otherwise        = (x:rest)
                       where rest = myDrop' (count+1) xs n

