--in order to use this, open ghci and
-- use wayTo and firstValueLarger



------------------------------------------
-- PART 1

rectLength :: Int -> Int
rectLength i = i * 2 + 1

firstNumOfRect :: Int -> Int
firstNumOfRect 0 = 1
firstNumOfRect i = rectLength (i - 1) ^ 2 + 1

rectCoords :: Int -> (Int, Int)
rectCoords i = (i, -i)

rectOf :: Int -> Int
rectOf 1 = 0
rectOf x = (head $ filter (\i -> firstNumOfRect i > x) [0..]) - 1

posOf :: Int -> (Int, Int)
posOf 1 = (0, 0)
posOf n = let rectIndex = rectOf n
              (x, y)    = rectCoords rectIndex
              firstNum  = firstNumOfRect rectIndex
              diff      = n - firstNum
              len       = rectLength rectIndex
              len1      = len - 1
              side      = diff `div` (len - 1)
              sideDiff  = diff `mod` (len - 1)
          in  case side of
               0 -> (x, y + sideDiff + 1)
               1 -> (x - sideDiff - 1, y + len1)
               2 -> (x - len1, y + len1 - sideDiff - 1)
               3 -> (x - len1 + sideDiff + 1, y)

wayTo :: Int -> Int
wayTo n = let (dx, dy) = posOf n
          in abs dx + abs dy

------------------------------------------------
-- PART 2

type Grid = [((Int, Int), Int)]

lastValue :: Int -> (Grid, Int)
lastValue 1 = let pos  = (0, 0)
                  cell = (pos, 1)
                  grid = [cell]
              in (grid, 1)
lastValue i = let (cells, _) = lastValue (i - 1)
                  (x0, y0)   = posOf i
                  dis x y    = abs (x0 - x) <= 1 && abs (y0 - y) <= 1
                  newValue   = sum [v | ((x, y), v) <- cells, dis x y] 
                  newCell     = ((x0, y0), newValue)
              in (newCell:cells, newValue)

firstValueLarger :: Int -> Int
firstValueLarger x = let values = map (snd . lastValue) [1..]
                         larger = filter (>x) values
                     in head larger
