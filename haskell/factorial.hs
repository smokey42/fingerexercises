facA :: Integer -> Integer
facA 0 = 1
facA 1 = 1
facA n = n * facA (n - 1)

facB :: Integer -> Integer
facB n = if n > 1
         then n * facB (n - 1)
         else 1
