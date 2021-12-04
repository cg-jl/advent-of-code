{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# OPTIONS_GHC -Wall #-}

import           Control.Arrow
import           Control.Monad
import           Data.Functor
import           Data.List
import           Data.Maybe

sample :: String
sample =
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n8  2 23  4 24\n21  9 14 16  7\n6 10  3 18  5\n1 12 20 15 19\n\n3 15  0  2 22\n9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n2  0 12  3  7"

-- get the call index for the nums
callIndex :: [Int] -> Int -> Maybe Int
callIndex nums target = elemIndex target nums <&> (+ 1)

processBoard :: [Int] -> Board -> Int -> Board
processBoard nums board turns = markNumbers (take turns nums) board

-- the minimum amount of turns needed to fill a line is the amount of turns that takes to complete the furthest value
turnsForLine :: [Int] -> [Int] -> Maybe (Int, Int)
turnsForLine nums =
  maximumBy (curry $ fmap fst *** fmap fst >>> uncurry compare) .
  map (\target -> callIndex nums target <&> (, target))

-- for a given group of lines, give the minimum turns and the last number that is called
turnsForLines :: [Int] -> [[Int]] -> Maybe (Int, Int)
turnsForLines nums = map (turnsForLine nums) >>> catMaybes >>> maxIfNempty
  where
    maxIfNempty [] = Nothing
    maxIfNempty xs =
      Just $ minimumBy (curry $ fst *** fst >>> uncurry compare) xs

-- minimum turns for a win. A `Nothing` represents that it doesn't win.
winTurns :: [Int] -> Board -> Maybe (Int, Int)
winTurns nums board = turnsForLines nums $ table ++ transpose table
  where
    table = onlyNumbers board

findLoser :: [Int] -> [Board] -> Maybe ((Board, Int), Int)
findLoser nums =
  zipWith (fmap . (,)) <*> map (winTurns nums) >>>
  catMaybes -- filter the boards that never win
             -- if not empty, get the max
   >>>
  doIfNempty (maximumBy (curry $ both (fst . snd) >>> uncurry compare) >>> tr)
  where
    tr (a, (b, c)) = ((a, b), c)

partTwo :: [Int] -> [Board] -> Maybe Int
partTwo nums =
  findLoser nums >>> fmap (first (uncurry $ processBoard nums) >>> score)

doIfNempty :: ([a] -> b) -> [a] -> Maybe b
doIfNempty f = uncons >>> fmap (uncurry (:) >>> f)

both :: Arrow a => a b c -> a (b, b) (c, c)
both = join (***)

onlyNumbers :: Board -> [[Int]]
onlyNumbers (Board table) = map (map fst) table

score :: (Board, Int) -> Int
score = first (unmarkedNumbers >>> sum) >>> uncurry (*)

unmarkedNumbers :: Board -> [Int]
unmarkedNumbers (Board b) = concat >>> filter (not . snd) >>> map fst $ b


findWinner :: [Int] -> [Board] -> Maybe ((Board, Int), Int)
findWinner nums =
  zipWith (fmap . (,)) <*> map (winTurns nums) >>>
  catMaybes -- remove the ones that don't win
   >>>
  doIfNempty (minimumBy (curry $ both (fst . snd) >>> uncurry compare) >>> tr)
  where
    tr (a, (b, c)) = ((a, b), c)

partOne :: [Int] -> [Board] -> Maybe Int
partOne nums =
  findWinner nums >>> fmap (first (uncurry $ processBoard nums) >>> score)

newtype Board =
  Board [[(Int, Bool)]]

instance Eq Board where
  Board a == Board b = map (map fst) a == map (map fst) b

markNumber :: Int -> Board -> Board
markNumber i (Board b) = Board $ map insert' b
  where
    insert' =
      map
        (\(a, x) ->
           if a == i
             then (a, True)
             else (a, x))

markNumbers :: [Int] -> Board -> Board
markNumbers = map markNumber >>> foldl (.) id

instance Show Board where
  show (Board elems) = "Board {\n" <> makeTable elems <> "}"
    where
      makeTable = map showRow >>> unlines >>> (<> "\x1b[m")
      showRow = map showN >>> unwords
      showN (n, False) = "\x1b[38;5;239m" <> shownum n
      showN (n, True)  = "\x1b[38;5;255m" <> shownum n
      shownum n
        | n < 10 = ' ' : show n
        | otherwise = show n

parse :: String -> ([Int], [Board])
parse =
  lines >>>
  groupBy (\_ a -> a /= "") >>>
  head &&& tail >>> -- separate the numbers list from the board data
  (head >>> parseNumbers) *** map (tail >>> parseBoard)
  where
    parseNumbers = sepBy (/= ',') >>> map read

sepBy :: (a -> Bool) -> [a] -> [[a]]
sepBy f =
  span f >>>
  second (maybeTail >>> fmap (sepBy f) >>> fromMaybe []) >>> uncurry (:)

maybeTail :: [a] -> Maybe [a]
maybeTail = uncons >>> fmap snd

parseBoard :: [String] -> Board
parseBoard = map (words >>> map read >>> map (, False)) >>> Board

loadContents :: IO ([Int], [Board])
loadContents = readFile "inputs/day4.txt" <&> parse

main :: IO ()
main = do
  (nums, boards) <- loadContents
  putStrLn "Part 1: "
  print $ partOne nums boards
  putStrLn "Part 2: "
  print $ partTwo nums boards
