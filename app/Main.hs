module Main where
import Control.Monad.State (State, state, evalState)
import System.Random (randomR, StdGen, newStdGen)
import Control.Monad (replicateM)

type RandState = State StdGen

main :: IO ()
main = do
    gen <- newStdGen
    maybeRolls <- getRolls
    case maybeRolls of
        Nothing -> putStrLn "Goodbye!"
        Just rolls -> do
            let results = evalState (mapM roll rolls) gen
            mapM_ print results
            putStrLn $ "Total:" ++ show (sum (map snd results))
            main

getRolls :: IO (Maybe [(Int, Int)])
getRolls = do
    putStrLn "Enter roll(s) separated by spaces (e.g. 3d6 1d8), or 'q' to quit:"
    rollInput <- getLine
    case rollInput of
        "q" -> pure Nothing
        _ -> case mapM parseRoll (words rollInput) of
            Nothing -> do
                putStrLn "Invalid input. Please enter rolls in the format NdS (e.g. 3d6)."
                getRolls
            Just validRolls -> pure (Just validRolls)

parseRoll :: String -> Maybe (Int, Int)
parseRoll input = case break (== 'd') input of
    (x, 'd':y) -> case (reads x, reads y) of
        ([(n, "")], [(s, "")]) -> Just (n, s)
        _ -> Nothing
    _ -> Nothing

roll :: (Int, Int) -> RandState ([Int], Int)
roll (n, s) = do
    rolls <- replicateM n (state $ randomR (1,s))
    return (rolls, sum rolls)