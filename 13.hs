import Data.List (nub)
import Data.Char (isDigit, isSpace)
import Control.Applicative (Alternative, (<|>))
import Control.Monad (ap)

type Dot = (Int,Int)
data Fold = X Int | Y Int deriving Show

fold :: Fold -> [Dot] -> [Dot]
fold f = nub . map (foldDot f)
  where foldDot (X x') (x,y) = (if x > x' then 2 * x' - x else x,y)
        foldDot (Y y') (x,y) = (x,if y > y' then 2 * y' - y else y)

folds :: [Fold] -> [Dot] -> [Dot]
folds (f:fs) = folds fs . fold f
folds []     = id

showDots ds = unlines [[if (x,y) `elem` ds then '#' else '.' | x <- [0..width]] | y <- [0..height]]
  where width = 1 + (maximum $ map fst ds)
        height = 1 + (maximum $ map snd ds)

answer = length . nub . (uncurry $ flip folds)

data Parser a = Parser (String -> (Maybe a, String))
parse (Parser p) = p
parseMaybe :: Parser a -> String -> Maybe a
parseMaybe (Parser pA) = fst . pA


charParser :: Parser Char
charParser = Parser $ \s -> if null s then (Nothing, s) else (Just $ head s, tail s)

-- Look at how this was implemented in the lab. and also check the library
predCharParser :: (Char -> Bool) -> Parser Char
predCharParser p = charParser >>= \c -> if p c then Parser (Just c,) else Parser ((Nothing,) . (c:))

digitParser :: Parser Char
digitParser = predCharParser isDigit

-- Look at how this was implemented in the lab. and also check the library
zeroOrMoreParser :: Parser a -> Parser [a]
zeroOrMoreParser pA = Parser $ f . parse pA
  where f (Nothing, s') = (Just [], s')
        f (Just a, s')  = (Just $ a:l,r)
	  where (Just l, r) = f $ parse pA s'

-- Look at how this was implemented in the lab. and also check the library
atLeastOneParser :: Parser a -> Parser [a]
atLeastOneParser pA = pA >>= \a -> (zeroOrMoreParser pA) >>= return . (a:)

-- Look at how this was implemented in the lab. and also check the library
textParser :: String -> Parser String
textParser text = let l = length text; in Parser $ \s -> if take l s == text then (Just text, drop l s) else (Nothing, s)

intParser :: Parser Int
intParser = (atLeastOneParser digitParser) >>= return . read

dotParser :: Parser Dot
dotParser = (,) <$> intParser <* textParser "," <*> intParser

spaceParser = predCharParser isSpace

foldXParser = textParser "fold along x=" >> X <$> intParser
foldYParser = textParser "fold along y=" >> Y <$> intParser
foldParser = foldXParser <|> foldYParser

chainParser :: Parser a -> Parser s -> Parser [a]
chainParser pA pS = (:) <$> pA <*> zeroOrMoreParser (pS *> pA)

inputParser :: Parser ([Dot], [Fold])
inputParser = (,) <$> dotsParser <* spaceParser <*> foldsParser
  where dotsParser = chainParser dotParser spaceParser
        foldsParser = chainParser foldParser spaceParser

instance Functor Parser where
  fmap f pA = pA >>= return . f
instance Applicative Parser where
  pure a = Parser (Just a,)
  (<*>) = ap
-- Look at how this was implemented in the lab. and also check the library
instance Monad Parser where
  return = pure
  (Parser pa) >>= f = Parser $ pb . pa
    where pb (Just a, s) = parse (f a) s
          pb (Nothing,s) = (Nothing,s)
-- Look at how this was implemented in the lab. and also check the library
instance Alternative Parser where
  (Parser pA1) <|> (Parser pA2) = Parser $ pA . pA1
    where pA (Just a1, s) = (Just a1, s)
          pA (Nothing, s) = pA2 s

