-- caesars cipher

import Data.Char


rotate_base :: Int -> Int -> Char -> Char
rotate_base base n = chr . (\x -> (x + n - base) `mod` 26 + base) . ord


rotate :: Int -> Char -> Char
rotate n char | char `elem` ['a'..'z'] = rotate_base (ord 'a') n char
              | char `elem` ['A'..'Z'] = rotate_base (ord 'A') n char


cipher :: Int -> [Char] -> [Char]
cipher n = map (rotate n)
