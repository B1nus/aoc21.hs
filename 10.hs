-- Imperative idea. (Look up recursive solution later)
-- Use a stack with each opening brace. When poping, check that
-- the end-brace matches the start brace on the stack.
import Data.List

data Delim = Bracket | Angle | Paren | Brace deriving (Eq,Show)
data Token = Start Delim | End Delim deriving (Eq, Show)
type Line = [Token] 

parseChar :: Char -> Token
parseChar c = case c of '(' -> Start Paren
                        '[' -> Start Bracket
                        '{' -> Start Brace
                        '<' -> Start Angle
                        ')' -> End Paren
                        ']' -> End Bracket
                        '}' -> End Brace
                        '>' -> End Angle

parseLine :: String -> Line
parseLine = map parseChar

data Res = Stack [Delim] | Illegal Delim Line deriving (Show)

illegal :: Line -> Res
illegal = illegal' []
  where illegal' s []                = Stack s
        illegal' s    ((Start d):ts) = illegal' (d:s) ts
        illegal' (s:ss) ((End d):ts) | s == d    = illegal' ss ts
                                     | otherwise = Illegal d ts

points :: Delim -> Int
points Paren   = 1
points Bracket = 2
points Brace   = 3
points Angle   = 4

calc (Stack ds) = calc' 0 ds
  where calc' p (d:ds) = calc' (p * 5 + points d) ds
        calc' p []     = p
calc _ = 0

