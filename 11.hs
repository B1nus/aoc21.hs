import Data.Maybe

data Cell = Flashed | Flash | Level Int
newtype Map = Map [[Cell]]

instance Show Cell where
  show Flashed   = "_"
  show Flash     = "*"
  show (Level l) = show l

instance Show Map where
  show (Map m) = unlines $ map (map $ head . show) m

parse :: String -> Map
parse = Map . map (map (Level . read . (:""))) . lines

isFlash Flash = True
isFlash _     = False
isFlashed Flashed = True
isFlashed _       = False
isNumber x (Level l) | x == l = True
isNumber _ _      = False

allPs :: Map -> [[(Int,Int)]]
allPs (Map m) = [[(x,y) | x <- xs] | y <- ys]
  where xs = [0..(length . head) m - 1]
        ys = [0..length m - 1]

map' f (Map m) = Map $ map (map f) m
count p (Map m) = sum . map (length . filter p) $ m

addOne :: Map -> Map
addOne = map' addOne'
  where addOne' Flashed = Flashed
        addOne' Flash   = Flash
        addOne' (Level n) | n >= 9    = Flash
                          | otherwise = Level $ n + 1

flashOnce :: Map -> Map
flashOnce (Map m) = Map $ zipWith (zipWith addCells) m (allAdj (Map m))
  where isFlash Flash = True
        isFlash _     = False
        addCells Flash i     = Flashed
        addCells (Level x) i | x + i > 9 = Flash
                             | otherwise = Level $ x + i
        addCells Flashed _   = Flashed
        allAdj = map (map countAdj) . allPs
        countAdj = length . filter isFlash . adj (Map m)

flash :: Map -> Map
flash m | count isFlash m > 0 = flash $ flashOnce m
        | otherwise           = m

step :: Map -> (Map, Int)
step m = (reset m', count isFlashed m')
  where m' = flash $ addOne m

reset :: Map -> Map
reset = map' reset'
  where reset' Flashed   = Level 0
        reset' (Level l) = Level l
        reset' Flash     = error "huh?"

steps :: Map -> Int -> (Map, Int)
steps map 0 = (map, 0)
steps map n = (finalMap, flashes + nextFlashed)
  where (nextMap, flashes) = step map
        (finalMap, nextFlashed) = steps nextMap (n - 1)

whenSync :: Map -> Int
whenSync = whenSync' 0
  where whenSync' n map | synced map = n
                        | otherwise  = whenSync' (n + 1) (fst $ step map)

synced :: Map -> Bool
synced (Map m) = all (all $ isNumber 0) m
-- firstFlash :: Map -> [[Int]]
-- firstFlash :: 
--
-- intervals :: Map -> [[Int]]
-- intervals = undefined
--
-- sync :: Map -> Int
-- sync = foldr lcm 1 . concat . intervals

adj m (x,y) = catMaybes [lvl m p | p <- ps, any (p `elem`) $ allPs m ]
  where ys = [(y-1)..(y+1)]
        xs = [(x-1)..(x+1)]
        ps = [(x',y') | x' <- xs, y' <- ys, not (x == x' && y == y')]

lvl (Map m) (x,y) | any ((x, y) `elem`) $ allPs (Map m) = Just $ m!!y!!x
                  | otherwise                           = Nothing
