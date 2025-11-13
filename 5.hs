import Data.List.Split (splitOn)
import Data.List (group, sort)

type Line = [(Int, Int)] -- Literally just all points in the line. Haskell is lazy so this is fine (: I love haskell!

parseLine = toLine . map (map read . splitOn ",") . splitOn " -> "
  where
    toLine :: [[Int]] -> Line
    toLine [[x1, y1], [x2, y2]] = [(x i, y i) | i <- [0..d]]
      where
        dx  = signum (x2 - x1)
        dy  = signum (y2 - y1)
        x i = x1 + dx * i
        y i = y1 + dy * i
        d   = max (abs (x2 - x1)) (abs (y2 - y1))

parse = map parseLine . lines

answer = sum
  . map (const 1)
  . filter (>= 2)
  . map length
  . group
  . sort
  . concat
  . parse

solve path = do
  s <- readFile path
  return $ answer s
