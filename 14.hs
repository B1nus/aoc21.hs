import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map, lookup, findMin, findMax, insertWith, insert, keys, empty, (!), elems)

-- Try making your own Map type.
type Pair = (Char,Char)
type Rules = Map Pair Char
type Pairs = Map Pair Int
type Chars = Map Char Int

insertRules :: Rules -> Pairs -> Chars -> (Pairs, Chars)
insertRules rules pairs chars = foldl applyRule (empty,chars) $ keys rules
  where applyRule (accPairs,accChars) rulePair = (newAccPairs, newAccChars)
          where prevPairCount = fromMaybe 0 $ lookup rulePair pairs
	        ruleChar = rules ! rulePair
		rulePairs = [fstPair,sndPair]
		  where fstPair = (fst rulePair, ruleChar)
		        sndPair = (ruleChar, snd rulePair)
		newAccPairs = foldl (\acc p -> insertWith (+) p prevPairCount acc) accPairs rulePairs
		newAccChars = insertWith (+) ruleChar prevPairCount accChars

insertRulesTimes n rules pairs chars | n <= 0    = (pairs,chars)
                                     | otherwise = insertRulesTimes (n - 1) rules pairs' chars'
  where (pairs',chars') = insertRules rules pairs chars

readPairs :: String -> Pairs
readPairs s = foldl (\ps p -> insertWith (+) p 1 ps) empty (zip s $ drop 1 s)

readRule :: String -> (Pair, Char)
readRule (c1:c2:cs) = (pair,char)
  where pair = (c1,c2)
        char = last cs

readChars :: String -> Chars
readChars = foldl (\cs c -> insertWith (+) c 1 cs) empty

readInput :: String -> (Pairs, Chars, Rules)
readInput s = (pairs,chars,rules)
  where (l1:_:ls) = lines s
        pairs = readPairs l1
	chars = readChars l1
	rules = foldl (\acc (p,c) -> insert p c acc) empty $ map readRule ls

maxMinusMin :: Chars -> Int
maxMinusMin map = max - min
  where max = maximum $ elems map
        min = minimum $ elems map

answer :: String -> Int
answer s = maxMinusMin chars'
  where (pairs,chars,rules) = readInput s
        (pairs',chars') = insertRulesTimes 10 rules pairs chars

