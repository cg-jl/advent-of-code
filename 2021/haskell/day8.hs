{-# LANGUAGE TupleSections, BangPatterns, FlexibleContexts #-}
import Control.Arrow
import Data.Functor
import Control.Monad.State
import Data.List
import qualified Data.Set as S
import Control.Monad
import Data.Maybe

sample :: String
sample = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n\
\edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n\
\fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\n\
\fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\n\
\aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\n\
\fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\n\
\dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\n\
\bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\n\
\egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\n\
\gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"

parseLine :: String -> ([String], [String])
parseLine =  break (=='|') >>> second tail >>> both (groupBy (\a b -> a /= ' ' && b /= ' ') >>> filter (/=" "))

both :: Arrow a => a b c -> a (b, b) (c, c)
both = join (***)

partOne :: [([String], [String])] -> Int
-- 1 takes 2, 7 takes 3, 4 takes 4, 8 takes 7
partOne = map  (snd >>> map length >>> filter (`elem` [2, 3, 4, 7]) >>> length) >>> sum

parse :: String -> [([String], [String])]
parse = lines >>> map parseLine



fLength :: Int -> [[a]] -> Maybe [a]
fLength x = find (length >>> (==x))

fRelative :: String -> Int -> [String] -> Maybe String
fRelative a i = find ((\\ a) >>> length >>> (== i))


-- get 1, 4, 7, 8 from their unique counts
stage1 :: MonadState [String] m => m (String, String, String, String)
stage1 = do
  xs <- get
  let Just !one = fLength 2 xs
      Just !four = fLength 4 xs
      Just !seven = fLength 3 xs
      Just !eight = fLength 7 xs
  modify' $ filter (\x -> x /= one && x /= four && x /= seven && x /= eight)
  return (one, four, seven, eight)

stage2 :: MonadState [String] m => String -> m (String, String)
stage2 seven = do
  xs <- get
  let Just !three = fRelative seven 2 xs
      Just !six   = fRelative seven 4 xs
  modify' $ filter $ \x -> x /= three && x /= six
  return (three, six)

-- Get two and zero from 3 and 4
stage3 :: MonadState [String] m => (String, String) -> m (String, String, String, String)
stage3 (three, four) = do
  (xs, rest) <- gets (partition ((\\ four) >>> length >>> (==3)))
  let [a, b] = xs
  let [c, d] = rest
  let lenA = length $ a \\ three
      lenC = length c
      (two, zero) = if lenA == 2 then (b, a) else (a, b)
      (five, nine) = if lenC == 6 then (d, c) else (c, d)

  return (two, zero, five, nine)


mkTable :: [String] -> [(S.Set Char, Int)]
mkTable = evalState $ do
  (one, four, seven, eight) <- stage1
  (three, six) <- stage2 seven
  (two, zero, five, nine) <- stage3 (three, four)
  return $ [ (zero, 0), (one, 1), (two, 2), (three, 3), (four, 4), (five, 5), (six, 6), (seven, 7), (eight, 8), (nine, 9) ] <&> first S.fromList


          
getValue :: [(S.Set Char, Int)] -> String -> Int
getValue table = S.fromList >>> flip lookup table >>> fromJust


decode :: [(S.Set Char, Int)] -> [String] -> Int
decode table = map (getValue table) >>> foldl (\x i -> x * 10 + i) 0

decodeLine :: ([String], [String]) -> Int
decodeLine (samples, code) = 
  let !table = mkTable samples
  in decode table code


partTwo :: [([String], [String])] -> Int
partTwo = map decodeLine >>> sum


main :: IO ()
main = do
  contents <- readFile "inputs/day8.txt" <&> parse
  putStrLn "Part 1:"
  print $ partOne contents
  putStrLn "Part 2:"
  print $ partTwo contents



