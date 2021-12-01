import Data.Maybe
import Data.List
import Data.Functor
import Control.Arrow

parse :: String -> [Int]
parse = words >>> map read


-- counts
countGreater :: Ord a => [a] -> Int
countGreater = zipWith (<) <*> safeTail >>> filter id >>> length



tripletSums :: Num c => [c] -> [c]
tripletSums = zipWith (+) . safeTail <*> safeTail . safeTail >>= zipWith (+)
safeTail :: [a] -> [a]
safeTail = uncons >>> fmap snd >>> fromMaybe []





main :: IO ()
main = do
    contents <- readFile "inputs/day1.txt" <&> parse
    putStrLn "Part 1: "
    print $ countGreater contents
    putStrLn "Part 2: "
    print $ countGreater $ tripletSums contents