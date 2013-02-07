data NestedList a = Elem a | List [NestedList a]

-- ??
myFlatten :: NestedList a -> [a]
myFlatten (Elem a) = [a]
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)
myFlatten (List []) = []


myPack :: [Char] -> [String]
myPack [] = []
myPack (x:xs) = [x : takeWhile (== x) xs] ++ myPack (dropWhile (== x) xs)


encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = (count, x) : encode (dropWhile (== x) xs)
              where count = 1 + length (takeWhile (== x) xs)


data EncodingEntry a = Single a | Multiple Int a
    deriving Show


toTuple :: EncodingEntry a -> (Int, a)
toTuple (Single a) = (1, a)
toTuple (Multiple n a) = (n, a)


encodeElement :: Int -> a -> EncodingEntry a
encodeElement 1 x = Single x
encodeElement n x = Multiple n x


encodeModified :: Eq a => [a] -> [EncodingEntry a]
encodeModified [] = []
encodeModified (x:xs) = (encodeElement count x): encodeModified (dropWhile (== x) xs)
                      where count = 1 + length (takeWhile (== x) xs)


encodeDirect :: Eq a => [a] -> [EncodingEntry a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirect' 1 x xs
encodeDirect' n y [] = [encodeElement n y]
encodeDirect' n y (x:xs) | y == x = encodeDirect' (n+1) y xs
                         | otherwise = encodeElement n y : (encodeDirect' 1 y xs)


decodeModified :: Eq a => [EncodingEntry a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = (take count $ repeat $ snd $ toTuple x) ++ decodeModified xs
                      where count = fst $ toTuple x


dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = (x:x:(dupli xs))


repli :: [a] -> Int -> [a]
repli [] n = []
repli (x:xs) n = replicate n x ++ (repli xs n)
