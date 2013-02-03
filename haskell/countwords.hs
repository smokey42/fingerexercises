import Data.Function
import Data.List
import qualified Data.Map as Map
import System.IO

countWords :: [String] -> [(String, Int)]
countWords words = reverse . sortBy (compare `on` snd) $
    Map.toList $
    foldl (\datamap word -> Map.insertWith (+) word 1 datamap) Map.empty words


showEntry :: (String, Int) -> String
showEntry (word, count) = word ++ ", " ++ show count


main :: IO ()
main = do
    content <- readFile "input.txt"
    putStrLn $ unlines $ map showEntry $ countWords $ words content
