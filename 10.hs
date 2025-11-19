-- Imperative idea. (Look up recursive solution later)
-- Use a stack with each opening brace. When poping, check that
-- the end-brace matches the start brace on the stack.

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
points Paren   = 3
points Bracket = 57
points Brace   = 1197
points Angle   = 25137

resToPoints (Stack _)   = 0
resToPoints (Illegal d ts) = points d

