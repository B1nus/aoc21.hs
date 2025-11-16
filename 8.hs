import Data.List.Split

count p = length . filter p -- I found this on hoogle as well, but it seems to
                            -- be quite obscure so I just defined it myself. (:
parse = map (words . drop 1 . dropWhile (/= '|')) . lines
unique x = length x `elem` [2, 3, 4, 7]

