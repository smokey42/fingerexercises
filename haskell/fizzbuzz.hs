fizzbuzzer :: Int -> String
fizzbuzzer i | fizz && buzz = "Fizzbuzz"
             | fizz = "Fizz"
             | buzz = "Buzz"
             | otherwise = show i
             where fizz = i `mod` 3 == 0
                   buzz = i `mod` 5 == 0

main = do putStrLn $ unlines $ map fizzbuzzer [1..100]
