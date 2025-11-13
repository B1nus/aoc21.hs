import Data.List.Split (splitOn)
import Data.List (group, sort)

type Line = [(Int, Int)] -- Literally just all points in the line. Haskell is lazy so this is fine (: I love haskell!

parseLine = toLine . map (map read . splitOn ",") . splitOn " -> "
  where
    toLine :: [[Int]] -> Line
    toLine [[x1, y1], [x2, y2]] | x1 == x2  = [(x1,y) | y <- [min y1 y2..max y1 y2]]
                                | y1 == y2  = [(x,y1) | x <- [min x1 x2..max x1 x2]]
                                | otherwise = []

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
