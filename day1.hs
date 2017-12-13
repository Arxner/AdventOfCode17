import Data.Char

main = do
      input <- getLine
      let i = map (digitToInt) input
      putStrLn $ show $ sum1 i
      putStrLn $ show $ sum2 i 0

--------------------------------------------------
-- PART 1

sum1 :: [Int] -> Int
sum1 (x:xs) = s (x:xs ++ [x])
            where s :: [Int] -> Int
                  s (x1:[]) = 0
                  s (x1:x2:xs) | x1 == x2  = x1 + s (x2:xs)
                               | otherwise =      s (x2:xs)

--------------------------------------------------
-- PART 2

sum2 :: [Int] -> Int -> Int
sum2 xs i | i < len   = (if a == b then a else 0)
                        + sum2 xs (i + 1)
          | otherwise = 0
            where len = length xs
                  off = (i + len `div` 2) `mod` len
                  a   = xs !! i
                  b   = xs !! off
