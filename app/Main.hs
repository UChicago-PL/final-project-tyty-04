module Main where
import Control.Monad.State (State, state, evalState)
import System.Random (randomR, StdGen, newStdGen)
import Control.Monad (replicateM)

main :: IO ()
main = do
    gen <- newStdGen
    input <- getLine 
    case parseRoll input of
        Just (n, s) -> print $ evalState (roll n s) gen
        Nothing -> putStrLn "Error"

type RandState = State StdGen

parseRoll :: String -> Maybe (Int, Int)
parseRoll input = case break (== 'd') input of
    (n, 'd':s) -> Just (read n, read s)
    _ -> Nothing

roll :: Int -> Int -> RandState [Int]
roll n s = replicateM n (state $ randomR (1,s))