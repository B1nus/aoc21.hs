import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (trace)
import Data.Tuple (swap)
import Data.Maybe (fromJust,isJust,catMaybes)

type Risk = Int
type Pos = (Int,Int)
type Cave = Map.Map Pos Risk
-- Try making a recursive data structure instead
-- Try making your own Priority Queue

readCave :: String -> Cave
readCave s = Map.fromList assocList
  where nums = map (map (read . (:""))) $ lines s
        assocList = zip [0..] nums >>= zipRow
	zipRow (y,row) = zipWith (\x n -> ((x,y),n)) [0..] row

risk :: Pos -> Cave -> Maybe Risk
risk = Map.lookup

end :: Cave -> Pos
end = maximum . Map.keys

insertManyWith :: Ord k => (v -> v -> v) -> Map.Map k v -> [(k,v)] -> Map.Map k v
insertManyWith f = foldl' $ flip $ uncurry $ Map.insertWith f

adj :: Pos -> Cave -> Map.Map Pos Risk
adj (x,y) cave = fromJust
	         <$> (Map.filter isJust
		 $ Map.fromList
		 $ map (\p -> (p,risk p cave))
		 $ [(x+1,y),(x-1,y),(x,y+1),(x,y-1)])

shortest :: Cave -> Int
shortest cave = shortest' cave $ Map.fromList [((0,0),0)]

shortest' :: Cave -> Map.Map Pos Int -> Int
shortest' cave shortestMap
 | p == end cave = fromJust $ Map.lookup p shortestMap
 | otherwise     = trace (show p ++ " " ++ show shortestMap) $ shortest' (Map.delete p cave) (Map.delete p shortestMap')
  where p = (snd
             . minimum
	     . map swap
	     . filter ((`Map.member` cave) . fst)
	     . Map.toList) shortestMap
	r = fromJust $ Map.lookup p shortestMap
        shortestMap' = insertManyWith min shortestMap $ map (\(p,d) -> (p,d + r)) $ Map.toList (adj p cave)

