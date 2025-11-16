import Data.List

unique x = length x `elem` [2, 3, 4, 7]

parse = map split . lines
  where
  known = map snd . sort . map (\w -> (length w, w)) . filter unique
  split s = (known $ words l, words $ drop 1 r)
    where
    (l, r) = break (=='|') s

digit [one',seven',four',eight'] s' | l == 6 && null (four \\ s)        = 9
                                    | l == 6 && null (one \\ s)         = 0
                                    | l == 6                            = 6
                                    | l == 5 && null (one \\ s)         = 3
                                    | l == 5 && length (four \\ s) == 2 = 2
                                    | l == 5                            = 5
                                    | s == one                          = 1
                                    | s == four                         = 4
                                    | s == seven                        = 7
                                    | s == eight                        = 8
  where
    l = length s
    s     = sort s'
    one   = sort one'
    seven = sort seven'
    four  = sort four'
    eight = sort eight'

output (known, [d1, d2, d3, d4]) = 1000 * f d1 + 100 * f d2 + 10 * f d3 + f d4
  where f = digit known

answer = sum . map output . parse

-- 1 has 2
-- 7 has 3
-- 4 has 4
-- 8 has 7
-- 2 has 5
-- 3 has 5
-- 5 has 5
-- 0 has 6
-- 6 has 6
-- 9 has 6

-- We know which digit it is for all lengths except for 5 and 6. In that case
-- we can follow the following algorithm to decide which digit is is:

-- If it has six lines it is either a 0, 6 or 9.
-- If it contains all letters in the four. it is a nine.
-- Otherwise if it contains two of the letter in one. it is a zero
-- otherwise it is a six.

-- If it has 5 lines it is either a 2, 3 or 5.
-- If it contains both lines in the one. It is a three.
-- Otherwise if it contains 2 of the lines in the four. It is a two.
-- Otherwise it is a five

