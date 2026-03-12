module Main where
import Control.Monad.State (State, state, evalState)
import System.Random (randomR, StdGen, newStdGen)
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Data.Ord (comparing)

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
        "roll" -> do 
            action <- doRoll stats
            case action of
                Quit -> putStrLn "Goodbye! Thank you for using Dice Roller."
                Return -> loop stats
                Result s -> loop s
        "stats" -> do
            action <- doStats stats
            case action of
                Quit -> putStrLn "Goodbye! Thank you for using Dice Roller."
                Return -> loop stats
                Result s -> loop s
        _ -> putStrLn "Invalid command." >> loop stats
    
doRoll :: StatMap -> IO (UserInput StatMap)
doRoll stats = do 
    maybeRolls <- getRolls
    case maybeRolls of
        Quit -> pure Quit
        Return -> pure (Result stats)
        Result rolls -> do
            gen1 <- newStdGen
            let roll = evalState (mapM rollDice rolls) gen1
            maybeMods <- getMods stats
            case maybeMods of
                Quit -> pure Quit
                Return -> pure (Result stats)
                Result mods -> do
                    let totalMods = sum $ map (resolveMod stats) mods
                    maybeFlags <- getFlags
                    case maybeFlags of
                        Quit -> pure Quit
                        Return -> pure (Result stats)
                        Result flag -> do
                            gen2 <- newStdGen
                            results <- case flag of
                                Just Adv -> do
                                    let roll2 = evalState (mapM rollDice rolls) gen2
                                    putStr "Roll 1: "
                                    printRolls roll
                                    putStr "Roll 2: "
                                    printRolls roll2
                                    putStrLn "Taking higher roll."
                                    pure $ if sum (map snd roll) >= sum (map snd roll2) then roll else roll2
                                Just Dis -> do
                                    let roll2 = evalState (mapM rollDice rolls) gen2
                                    putStr "Roll 1: "
                                    printRolls roll
                                    putStr "Roll 2: "
                                    printRolls roll2
                                    putStrLn "Taking lower roll."
                                    pure $ if sum (map snd roll) <= sum (map snd roll2) then roll else roll2
                                Just (Drop x y) -> do
                                    let allRolls = evalState (replicateM x (mapM rollDice rolls)) gen2
                                    let sorted = sortBy (comparing ( sum . map snd)) allRolls
                                    mapM_ (\(i, pool) -> do
                                        putStr $ "Roll " ++ show i ++ ": "
                                        printRolls pool
                                        ) (zip [1 :: Int ..] allRolls)
                                    putStrLn $ "Dropping lowest " ++ show y ++ " roll(s)."
                                    pure $ concat $ drop y sorted
                                Nothing -> do
                                    putStr "Roll: "
                                    printRolls roll
                                    pure roll
                            putStrLn $ "Result: " ++ show (sum (map snd results) + totalMods)
                            doRoll stats

doStats :: StatMap -> IO (UserInput StatMap)
doStats stats = do
    action <- getStats stats
    case action of
        Quit -> pure Quit
        Return -> pure (Result stats)
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
        "" -> do
            putStrLn "Please enter at least one roll."
            getRolls
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
    putStrLn "Enter stat assignments in the form 'stat=val' separated by spaces (e.g. str=15 dex=12). Enter 'roll' to generate 6 values using the standard roll 4d6 drop 1 method. Enter 'r' to return to main menu or 'q' to quit:"
    statInput <- getLine
    case statInput of
        "q" -> pure Quit
        "r" -> pure Return
        "" -> do
            putStrLn "Please enter at least one stat assignment."
            getStats stats
        "roll" -> do
            gen <- newStdGen
            let rolls = evalState (replicateM 6 (rollDice (4, 6))) gen
            let genStats = map (\(r, _) -> sum r - minimum r) rolls
            putStrLn $ "Generated stats: " ++ show genStats
            putStrLn "Note: This doesn't automatically change your stats, it's up to you to assign these values how you wish."
            getStats stats
        _ -> case mapM (parseStats stats) (words statInput) of
            Nothing -> do
                putStrLn "Invalid input."
                getStats stats
            Just validStats -> pure (Result validStats)

-- Parse a stat assignment in the form stat=val and return the stat name and value or nothing
parseStats :: StatMap -> String -> Maybe (String, Int)
parseStats stats input = case break (== '=') input of
    (stat, '=':val) | isValidStat stats stat -> case reads val of
        [(n, "")] -> Just (stat, n)
        _ -> Nothing
    _ -> Nothing

-- Check if a stat is valid
isValidStat :: StatMap -> String -> Bool
isValidStat stats s = Map.member s stats

data Mod = FlatMod Int | StatMod String deriving (Show)

-- Get modifiers from user input in the format +N or -N
getMods :: StatMap -> IO (UserInput [Mod])
getMods stats = do
    putStrLn "Enter modifiers separated by spaces (e.g. +2 -1 +str), or press Enter to skip. Enter 'r' to return to main menu or 'q' to quit:"
    modInput <- getLine
    case modInput of
        "q" -> pure Quit
        "r" -> pure Return
        _ -> case mapM parseMod (words modInput) of
            Nothing -> do
                putStrLn "Invalid input."
                getMods stats
            Just validMods -> case mapM (validateMod stats) validMods of
                Nothing -> do
                    putStrLn "Invalid input."
                    getMods stats 
                Just _ -> pure (Result validMods)

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

-- Validate that all state modifiers are valid stats
validateMod :: StatMap -> Mod -> Maybe Mod
validateMod stats (StatMod s) 
    | isValidStat stats s = Just (StatMod s)
    | otherwise = Nothing
validateMod _ m = Just m

data Flag = Adv | Dis | Drop Int Int deriving (Show, Eq)

-- Get flags from user input in the format flag1 flag2 ...
getFlags :: IO (UserInput (Maybe Flag))
getFlags = do
    putStrLn "Enter a flag, or press Enter to skip. Enter 'h' for a list of flags, 'r' to return to main menu, or 'q' to quit:"
    flagInput <- getLine
    case flagInput of
        "q" -> pure Quit
        "r" -> pure Return
        "h" -> do
            putStrLn "Available flags:"
            putStrLn "- 'adv': Roll with advantage (take the higher of two rolls)"
            putStrLn "- 'dis': Roll with disadvantage (take the lower of two rolls)"
            putStrLn "- 'rXdY': Repeat the roll X times, drop the lowest Y totals, and combine the remaining rolls"
            getFlags
        "" -> pure (Result Nothing)
        _ -> case validateFlag flagInput of
            Nothing -> do
                putStrLn "Invalid input."
                getFlags
            Just validFlag -> pure (Result (Just validFlag))

-- Validate that entered flags are correct
validateFlag :: String -> Maybe Flag
validateFlag "adv" = Just Adv
validateFlag "dis" = Just Dis
validateFlag ('r':rest) = case break (== 'd') rest of
    (x, 'd':y) -> case (reads x, reads y) of
        ([(r, "")], [(d, "")]) | d < r -> Just (Drop r d)
        _ -> Nothing
    _ -> Nothing
validateFlag _ = Nothing 

printRolls :: [([Int], Int)] -> IO ()
printRolls rolls = do
    let allRolls = concatMap fst rolls
    let total = sum (map snd rolls)
    putStrLn $ show allRolls ++ " Total: " ++ show total

-- Roll N dice with S sides and return the individual rolls and their total
rollDice :: (Int, Int) -> RandState ([Int], Int)
rollDice (n, s) = do
    rolls <- replicateM n (state $ randomR (1,s))
    return (rolls, sum rolls)