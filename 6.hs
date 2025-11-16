import Data.List.Split

parseList :: String -> [Int]
parseList = map read . splitOn ","

translate xs = [f i xs | i <- [0..8]]
  where
    f x = sum . map (const 1) . filter (==x)

-- Instead of modelling each number. We can model the number of fish with any given timer
nextDay [a,b,c,d,e,f,g,h,i] = [b,c,d,e,f,g,h + a,i,a]

simulate fs 0         = fs
simulate fs d | d > 0 = simulate (nextDay fs) (d - 1)

answer s = sum $ simulate (translate $ parseList s) 80

