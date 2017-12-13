import qualified Data.Map.Strict as Map

main = do
     lines <- readLines
     putStrLn $ show $ amountValid (not . hasDuplicates) lines
     putStrLn $ show $ amountValid (not . hasAnagram) lines


readLines :: IO [String]
readLines = do
            line <- getLine
            if line == ""
            then return []
            else do next <- readLines
                    return (line:next)
                        
-------------------------------------------------
-- PART 1


amountValid :: (String -> Bool) -> [String] -> Int
amountValid p xs = length $ filter (id) $ map p xs

hasDuplicates :: String -> Bool
hasDuplicates l = let ws = words l
                      hasDuplicates' :: [String] -> Int -> Bool
                      hasDuplicates' [] _                  = False
                      hasDuplicates' xs i | i >= length xs = False
                                          | otherwise      = (xs !! i) `elem` (drop n xs) || 
                                               hasDuplicates' xs n
                                          where n = i + 1
                  in hasDuplicates' ws 0

-------------------------------------------------
-- PART 2

hasAnagram :: String -> Bool
hasAnagram l = let ws = words l
                   hasAnagram' :: [String] -> Int -> Bool
                   hasAnagram' [] _                  = False
                   hasAnagram' xs i | i >= length xs = False
                                    | otherwise      = any (anagram (xs !! i)) (drop n xs) ||
                                             hasAnagram' xs n
                                    where n = i + 1
               in hasAnagram' ws 0


anagram :: String -> String -> Bool
anagram a b | length a /= length b = False
            | otherwise            = 
                let amount :: Char -> String -> Int
                    amount c s = length $ filter (== c) s
                    c1 = map (\x -> (x, amount x a)) a
                    c2 = map (\x -> (x, amount x b)) b
                in Map.fromList c1 == Map.fromList c2

