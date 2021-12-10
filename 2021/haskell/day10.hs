{-# LANGUAGE FlexibleContexts #-}
import Control.Arrow
import Data.Traversable
import Control.Applicative
import Control.Monad.Except
import Data.Functor
import System.Exit
import System.IO
import System.Environment
import Data.Maybe
import Data.List
import Control.Monad.State
import Data.Foldable
import Data.Either

usage :: String -> IO ()
usage progname = putStrLn $ "usage: " ++ progname ++ " <file>"

counterpart :: Char -> Char
counterpart '<' = '>'
counterpart '(' = ')'
counterpart '[' = ']'
counterpart '{' = '}'

-- get a line and output either an autocomplete or a syntax error
checkLine :: String -> Either Char String
checkLine = flip evalState "" . runExceptT . checkLine'
    where checkLine' = traverse_ checkChar >=> const get
          checkChar x
            | x `elem` "<([{" = modify' (counterpart x :)
            | otherwise = do
                -- pop the last opening, if it was empty or it's not the expected one
                -- then say that the character is the problem. Put the rest to make sure it was popped.
                current <- gets uncons
                case current of
                  Nothing -> throwError x
                  Just (c, cs) -> if c /= x then throwError x else put cs


errorScore :: String -> Int
errorScore = mapMaybe score  >>> sum
    where score ')' = Just 3 
          score ']' = Just 57
          score '}' = Just 1197
          score '>' = Just 25137
          score _ = Nothing


autocompleteScore :: String -> Int
autocompleteScore = mapMaybe score >>> foldl (\x a -> x * 5 + a) 0
    where score ')' = Just 1
          score ']' = Just 2
          score '}' = Just 3
          score '>' = Just 4
          score _ = Nothing


completionScore :: [String] -> Int
completionScore = map autocompleteScore >>> sort >>> half
    where half xs = xs !! (length xs `div` 2)

day10 :: String -> (Int, Int)
day10 = lines >>> map checkLine >>> partitionEithers >>> errorScore *** completionScore

main :: IO ()
main = do
    progname <- getProgName
    name <- (getArgs <&> uncons) >>= maybe (usage progname >> exitFailure) return . fmap fst
    (part1, part2) <- readFile name <&> day10
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
