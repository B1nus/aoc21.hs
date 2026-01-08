import Test.QuickCheck
import Data.Char
import Data.List
import Debug.Trace

type Cave = String
type Edge = (Cave, Cave)
type Graph = [Edge]
type Path = [Cave]

adjacentFromCave :: Cave -> Graph -> [Cave]
adjacentFromCave c = map (\e -> if fst e == c then snd e else fst e) . filter (\e -> fst e == c || snd e == c)

counts :: Ord a => [a] -> [(a,Int)]
counts =  map (\x -> (head x, length x)) . group . sort

isValidPartialPath :: Path -> Bool
isValidPartialPath path = ("start", 1) `elem` lowerCounts &&
                          length (filter ((==2) . snd) lowerCounts) <= 1 &&
                          maximum (map snd lowerCounts) <= 2
  where lowerCounts = filter (isLower . head . fst) $ counts path

isValidPath :: Path -> Bool
isValidPath path = isValidPartialPath path && head path == "end"

pathsFrom :: Path -> Graph -> Int
pathsFrom path graph | isValidPath path = 1
                     | isValidPartialPath path = sum $ map (\adj -> pathsFrom (adj:path) graph) $ adjacentFromCave (head path) graph
                     | otherwise = 0

paths = pathsFrom ["start"]

readGraph :: String -> Graph
readGraph = map readEdge . lines

readEdge :: String -> Edge
readEdge s = (from, tail to)
  where (from,to) = break (== '-') s
