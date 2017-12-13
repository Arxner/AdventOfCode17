import Data.Char

main = do
         input <- readSheet
         putStrLn $ show $ sum $ map (chk1) input
         putStrLn $ show $ sum $ map (chk2) input

-- Reads a spreadsheet from the terminal
readTerminal :: IO [String]
readTerminal = do
            input <- getLine
            let result = if input == "" 
                 then return [] 
                 else do 
                       next <- readTerminal
                       return (input:next)
            result

readSheet :: IO [[Int]]
readSheet = do lines <- readTerminal
               let ws = map (words) lines
               return $ map (map (read)) ws

-----------------------------------------
-- PART 1

chk1 :: [Int] -> Int
chk1 r = abs (minimum r - maximum r)

-----------------------------------------
-- PART 2

-- Yes, this one operates on a pair!
divable (a,b) = a `mod` b == 0

chk2 :: [Int] -> Int
chk2 xs = fst fdiv `div` snd fdiv
         where fdiv = divs !! 0
               divs  = filter (divable) pairs
               pairs = [(fst a, fst b) | a <- xsi, b <- xsi, snd a /= snd b]
               xsi      = zip xs [0..]
