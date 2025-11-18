import Data.List

type Map = [[Int]]

parse :: String -> Map
parse = map (map (read . (:""))) . lines

w :: Map -> Int
w = length . head

h :: Map -> Int
h = length

risk :: Map -> (Int, Int) -> Int
risk m (x,y) = if ins m (x,y) then m!!y!!x else 11

ins :: Map -> (Int, Int) -> Bool
ins m (x, y) = x `elem` [0..w m - 1] && y `elem` [0..h m - 1]

adj :: Map -> (Int, Int) -> [(Int, Int)]
adj m (x, y) = filter (ins m) [(x - 1, y),(x,y - 1),(x + 1,y),(x,y + 1)]

isLower m p = (risk m p <) . risk m
isHigher m p = (risk m p >) . risk m

lows :: Map -> [(Int, Int)]
lows m = [(x, y) | x <- xs, y <- ys, isLow m (x, y)]
  where xs          = [0..(w m - 1)]
        ys          = [0..(h m - 1)]
        isLow m p = all (isLower m p) $ adj m p

basin :: Map -> (Int, Int) -> [(Int, Int)]
basin m (x,y) = basin' [(x,y)]
  where basin' ps | ps' == ps = ps
                  | otherwise = basin' ps'
          where ps'    = nub $ new ps ++ ps
                new ps = concat [new' (x,y) | (x,y) <- ps]
                new' p = filter ((/= 9) . risk m) $ filter (isLower m p) $ adj m p

