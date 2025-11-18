type Map = [[Int]]

parse :: String -> Map
parse = map (map (read . (:""))) . lines

w :: Map -> Int
w = length . head

h :: Map -> Int
h = length

risk :: Map -> Int -> Int -> Int
risk m x y | x >= 0 && x < w m && y >= 0 && y < h m = 1 + m!!y!!x
           | otherwise                              = 11

isLow :: Map -> Int -> Int -> Bool
isLow m x y = c < l && c < r && c < t && c < b
  where c = risk m x y
        l = risk m (x - 1) y
        r = risk m (x + 1) y
        t = risk m x (y - 1)
        b = risk m x (y + 1)

lows :: Map -> [Int]
lows m = [risk m x y | x <- xs, y <- ys, isLow m x y]
  where xs = [0..(w m - 1)]
        ys = [0..(h m - 1)]
