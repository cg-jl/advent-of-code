import           Control.Arrow
import           Control.Monad
import           Data.Bits
import           Data.Char
import           Data.Function
import           Data.Functor
import           Data.List
import qualified Data.Map      as Map

parseList :: String -> [String]
parseList = lines

countFreqs :: Ord a => [a] -> [(Int, a)]
countFreqs = sort >>> group >>> map (length &&& head)

maxFreq :: Ord a => [a] -> a
maxFreq =
  countFreqs >>> maximumBy (curry $ both fst >>> uncurry compare) >>> snd

minFreq :: Ord a => [a] -> a
minFreq =
  countFreqs >>> minimumBy (curry $ both fst >>> uncurry compare) >>> snd

both :: Arrow a => a b c -> a (b, b) (c, c)
both = join (***)

toNumber :: String -> Int
toNumber = map (\x -> ord x - ord '0') >>> foldl (\x y -> x `shiftL` 1 .|. y) 0

partOne :: [String] -> Int
partOne = getMaxMins >>> both toNumber >>> uncurry (*)

getMaxMins :: [String] -> (String, String)
getMaxMins = transpose >>> map maxFreq &&& map minFreq

-- make a filter that will process the indicated column with the right frequency analyzer
mkFilter :: Eq a => (Int -> [[a]] -> a) -> Int  -> [[a]] -> [[a]]
mkFilter f col = f col >>= filterByCol col

-- make the set of filters that will process the input with each column
mkFilters :: Eq a => (Int -> [[a]] -> a) -> [b] -> [[[a]] -> [[a]]]
mkFilters f = map (mkFilter f) . indices

rates :: Eq a => (Int -> [[a]] -> a) -> [[a]] -> [[a]]
rates f = id &&& mkFilters f >>> uncurry (foldl (&))


-- mkFreqs :: (Int -> b) -> [a] -> [b]
-- mkFreqs f =  map f . indices
-- getRating :: [[String] -> String] -> [String] -> Int
-- getRating _ [x] = toNumber x -- we've finished as only one remains
-- getRating (f : freq) xs = (getRating freqs $ freqs >>= keepSame) xs
colMax :: Ord a => Int -> [[a]] -> a
colMax col = map (!! col) >>> maxFreq

colMin :: Ord a => Int -> [[a]] -> a
colMin col = map (!! col) >>> minFreq

filterByCol :: Eq a => Int -> a -> [[a]] -> [[a]]
filterByCol i a [x] = [x]
filterByCol i a xs = filter ((!! i) >>> (== a)) xs

-- keep the ones that have the same values as [a]
keepSame :: Eq a => [a] -> [[a]] -> [[a]]
keepSame = zipWith filterByCol [0 ..] >>> foldr (.) id

indices :: [a] -> [Int]
indices = zipWith const [0 ..]

-- sample :: [String]
-- sample =
--   parseList
--     "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"

partTwo :: [String] -> Int
partTwo = oxrates &&& co2rates >>> both head >>> both toNumber >>> uncurry (*)
    where oxrates = rates colMax
          co2rates = rates colMin

-- load the official list
loadList :: IO [String]
loadList = readFile "inputs/day3.txt" <&> parseList


main :: IO ()
main = do
    l <- loadList
    putStrLn "Part 1: "
    print $ partOne l
    putStrLn "Part 2: "
    print $ partTwo l



