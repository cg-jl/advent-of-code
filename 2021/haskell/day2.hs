{-# LANGUAGE FlexibleContexts #-}
import Control.Arrow
import Data.List
import Data.Functor
import Control.Monad.State

data Action = Forward | Up | Down
    deriving Show



parseAction :: String -> (Action, Int)
parseAction = span (/= ' ') >>> readAction *** (tail >>> read)
    where readAction "forward" = Forward
          readAction "up" = Up
          readAction "down" = Down

parseFile :: String -> [(Action, Int)]
parseFile = lines >>> map parseAction

partOneStep :: MonadState (Int, Int) m => (Action, Int) -> m ()
partOneStep (Forward, value) = modify (first (+value))
partOneStep (Up, value) = modify (second (\x -> x -value))
partOneStep (Down, value) = modify (second (+value))

partTwoStep :: MonadState ((Int, Int), Int) m => (Action, Int) -> m ()
partTwoStep (Forward, value) = do aim <- gets snd
                                  modify $ first $ (+value) *** (+(value * aim))
partTwoStep (Up, value) = modify $ second (\x ->  x -value)
partTwoStep (Down, value) = modify $ second (+value)




partOne :: [(Action, Int)] -> Int
partOne = evalRun partOneStep (0, 0) >>> uncurry (*)

partTwo :: [(Action, Int)] -> Int
partTwo = evalRun partTwoStep ((0, 0), 0) >>> fst >>> uncurry (*)

evalRun :: ((Action, Int) -> State s a) -> s ->  [(Action, Int)] -> s
evalRun f = flip (execState . traverse f)




main :: IO ()
main = do
    contents <- readFile "inputs/day2.txt" <&> parseFile
    putStrLn "Part 1: "
    print $ partOne contents
    putStrLn "Part 2: "
    print $ partTwo contents