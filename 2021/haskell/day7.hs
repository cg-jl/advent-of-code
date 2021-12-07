import Control.Applicative
import Control.Arrow
import Data.Functor
import Data.List

parse :: String -> [Int]
parse = groupBy (\a b -> a /= ',' && b /= ',') >>> snd . partition (== ",") >>> map read

sample :: String
sample = "16,1,2,0,4,2,7,1,2,14"

distance :: Int -> Int -> Int
distance a b = abs $ a - b

-- sum of the N first integers of an arithmetic progression
fuel :: Int -> Int -> Int
fuel a b = let n = distance a b in (n + n * n) `div` 2

partOne :: [Int] -> Int
partOne = zipWith (map . distance) <*> repeat >>> map sum >>> minimum

bounds :: (Traversable t, Ord c, Enum c) => t c -> [c]
bounds = minimum &&& maximum >>> uncurry enumFromTo

sumFuels :: Int -> [Int] -> Int
sumFuels f = map (fuel f) >>> sum

ap :: Applicative f => f (a -> b) -> f a -> f b
ap = (<*>)

partTwo :: [Int] -> Int
partTwo = liftA2 map (flip sumFuels) bounds >>> minimum

main :: IO ()
main = do
  contents <- readFile "inputs/day7.txt" <&> parse
  putStrLn "Part 1:"
  print $ partOne contents
  putStrLn "Part 2: "
  print $ partTwo contents
