import Data.List (transpose, sort)
import Data.List.Split (splitOn)

-- Useful type aliases
data Elem  = Marked | Num Int deriving Show
type Row   = [Elem]
type Board = [Row]

mark :: Board -> Int -> Board
mark b n = [[markElem e | e <- row] | row <- b]
  where
    markElem (Num e) | n == e = Marked
    markElem (Num e)          = Num e
    markElem _                = Marked

marked Marked = True
marked _      = False

unwrap Marked  = 0
unwrap (Num n) = n

won b = w b || w (transpose b)
  where
    w = any $ all marked

play :: Board -> [Int] -> (Int, Int)
play = play' 0
  where
    play' acc b (n:ns) | won b'    = (acc + 1, score b' n)
                       | otherwise = play' (acc + 1) b' ns
      where b' = mark b n

score :: Board -> Int -> Int
score b n = n * sum (map unwrap $ concat b)

solve :: [Int] -> [Board] -> Int
solve ls bs = snd $ minimum $ map (`play` ls) bs

parse :: String -> ([Int], [Board])
parse s = (parseList l, parseBoards r)
  where
    (l:r) = splitOn "\n\n" (take (length s - 1) s)
    parseList = map read . splitOn ","
    parseBoards = map parseBoard
    parseBoard = map parseRow . splitOn "\n"
    parseRow = map (Num . read) . words

answer :: FilePath -> IO Int
answer path = do
  i <- readFile path
  let (ls, bs) = parse i
  return $ solve ls bs

