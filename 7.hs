import Data.List.Split
-- We are trying to minimize. c x = sum [abs $ x1 - x, abs $ x2 - x, ... ]
-- maybe there's some smart way to do it.

-- For now I'll just assume that the avarage value is the cheapest. This is likely wrong.
-- Update: It was wrong. very wrong. ):
--
-- Fuck it, let's just do brute force.

parseList = map read . splitOn ","
average xs = sum xs `div` length xs
cost x = sum . map (\a -> abs (a - x)) -- :: Integer -> [Integer] -> Integer
answer i = snd $ minimum $ map (\x -> (x `cost` xs, x)) [l..r]
  where
    xs = parseList i
    l = minimum xs
    r = maximum xs

