module Main where
import Control.Monad.State (State, state, evalState)
import System.Random (randomR, StdGen, newStdGen)
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map
import System.Exit (exitSuccess)

type RandState = State StdGen
data UserInput x = Quit | Return | Result x deriving (Show)
type StatMap = Map.Map String Int

defaultStats :: StatMap
defaultStats = Map.fromList [("str", 10), ("dex", 10), ("con", 10), ("int", 10), ("wis", 10), ("cha", 10)]

main :: IO ()
main = loop defaultStats

loop :: StatMap -> IO ()
loop stats = do
    putStrLn "Welcome to Dice Roller! Enter 'roll' to roll dice or 'stats' to set stats. At any time enter 'r' to return to this menu or 'q' to quit."
    cmd <- getLine 
    case cmd of
        "q" -> putStrLn "Goodbye! Thank you for using Dice Roller."
        "r" -> loop stats
        "roll" -> doRoll stats >>= loop 
        "stats" -> doStats stats >>= loop
        _ -> putStrLn "Invalid command." >> loop stats
    
doRoll :: StatMap -> IO StatMap
doRoll stats = do 
    maybeRolls <- getRolls
    case maybeRolls of
        Quit -> putStrLn "Goodbye! Thank you for using Dice Roller." >> exitSuccess
        Return -> pure stats
        Result rolls -> do
            gen <- newStdGen
            let results = evalState (mapM roll rolls) gen
            maybeMods <- getMods
            case maybeMods of
                Quit -> putStrLn "Goodbye! Thank you for using Dice Roller." >> exitSuccess
                Return -> pure stats
                Result mods -> do
                    let totalMods = sum $ map (resolveMod stats) mods
                    putStrLn "Results:" 
                    mapM_ print results
                    putStrLn $ "Total: " ++ show (sum (map snd results) + totalMods)
                    pure stats

doStats :: StatMap -> IO StatMap
doStats stats = do
    action <- getStats stats
    case action of
        Quit -> putStrLn "Goodbye! Thank you for using Dice Roller." >> exitSuccess
        Return -> pure stats
        Result validStats -> do
            let newStats = foldr (\(k, v) m -> Map.insert k v m) stats validStats
            mapM_ print (Map.toList newStats)
            doStats newStats

-- Get rolls from user input in the format NdS, where N is the number of dice and S is the number of sides
getRolls :: IO (UserInput [(Int, Int)])
getRolls = do
    putStrLn "Enter roll(s) separated by spaces (e.g. 3d6 1d8). Enter 'r' to return to main menu or 'q' to quit:"
    rollInput <- getLine
    case rollInput of
        "q" -> pure Quit
        "r" -> pure Return
        _ -> case mapM parseRoll (words rollInput) of
            Nothing -> do
                putStrLn "Invalid input."
                getRolls
            Just validRolls -> pure (Result validRolls)

-- Parse a single roll in NdS format into a tuple (N, S)
parseRoll :: String -> Maybe (Int, Int)
parseRoll input = case break (== 'd') input of
    (x, 'd':y) -> case (reads x, reads y) of
        ([(n, "")], [(s, "")]) -> Just (n, s)
        _ -> Nothing
    _ -> Nothing

-- Get stat assignments from user in the form of stat val
getStats :: StatMap -> IO (UserInput [(String, Int)])
getStats stats = do
    putStrLn "Enter stat assignments in the form 'stat=val' separated by spaces (e.g. str=15 dex=12). Enter 'r' to return to main menu or 'q' to quit:"
    statInput <- getLine
    case statInput of
        "q" -> pure Quit
        "r" -> pure Return
        _ -> case mapM parseStats (words statInput) of
            Nothing -> do
                putStrLn "Invalid input."
                getStats stats
            Just validStats -> pure (Result validStats)

-- Parse a stat assignment in the form stat=val and return the stat name and value or nothing
parseStats :: String -> Maybe (String, Int)
parseStats input = case break (== '=') input of
    (stat, '=':val) -> case reads val of
        [(n, "")] -> Just (stat, n)
        _ -> Nothing
    _ -> Nothing

data Mod = FlatMod Int | StatMod String deriving (Show)

-- Get modifiers from user input in the format +N or -N
getMods :: IO (UserInput [Mod])
getMods = do
    putStrLn "Enter modifiers separated by spaces (e.g. +2 -1 +str). Enter 'r' to return to main menu or 'q' to quit:"
    modInput <- getLine
    case modInput of
        "q" -> pure Quit
        "r" -> pure Return
        _ -> case mapM parseMod (words modInput) of
            Nothing -> do
                putStrLn "Invalid input."
                getMods 
            Just validMods -> pure (Result validMods)

-- Parse a single modifier in +N or -N format into an integer
parseMod :: String -> Maybe Mod
parseMod input = case input of
    ('+':x) -> case reads x of
        [(n, "")] -> Just (FlatMod n)
        [] -> Just (StatMod x)
        _ -> Nothing
    ('-':x) -> case reads x of
        [(n, "")] -> Just (FlatMod (-n))
        _ -> Nothing
    _ -> Nothing

-- Return the flat modifier or the stat modifier for a given stat
resolveMod :: StatMap -> Mod -> Int 
resolveMod _ (FlatMod n) = n
resolveMod stats (StatMod s) = (stats Map.! s - 10) `div` 2

-- Roll N dice with S sides and return the individual rolls and their total
roll :: (Int, Int) -> RandState ([Int], Int)
roll (n, s) = do
    rolls <- replicateM n (state $ randomR (1,s))
    return (rolls, sum rolls)