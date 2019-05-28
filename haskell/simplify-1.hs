import Data.Char
import Data.List
import Control.Applicative (Alternative (..))

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f p = Parser (\s -> parse p s >>= \(v, r) -> return (f v, r)) 

instance Applicative Parser where
  pure v = Parser (\s -> Just (v, s))
  fp <*> p = Parser (\s -> do (f, r) <- parse fp s
                              (v, t) <- parse p r
                              return (f v, t))
    
instance Alternative Parser where
  empty = Parser (\s -> Nothing)
  p <|> q = Parser (\s -> parse p s <|> parse q s)

instance Monad Parser where
  m >>= f = Parser (\s -> parse m s >>= \(v, r) -> parse (f v) r)

item :: Parser Char
item = Parser (\s -> case s of { [] -> empty; (c : cs) -> pure (c, cs) })

match :: Parser a -> (a -> Bool) -> Parser a
match m p = m >>= \t -> if p t then return t else empty

number :: Parser Int
number = some (match item isDigit) >>= \ds -> return (read ds :: Int)

variables :: Parser String
variables = some (match item isAlpha)

isSign :: Char -> Bool
isSign c = c == '+' || c == '-'

sign :: Parser Int
sign = match item isSign >>= \s -> return (case s of { '+' -> 1; '-' -> -1 })

monom :: Parser (String, Int)
monom = do s <- sign
           n <- number <|> pure 1
           v <- variables <|> pure ""
           return (sort v, s * n)

polyParse :: String -> [(String, Int)]
polyParse [] = []
polyParse s@(c : cs)
  = case parse (some monom) (if isSign c then s else '+' : s) of 
      Just (v, s) -> v
      Nothing -> error ("cannot parse: " ++ s)

monocmp :: (String, Int) -> (String, Int) -> Ordering
monocmp (m, i) (n, j) = let c = compare (length m) (length n)
                        in if c == EQ then compare m n else c

monosame :: (String, Int) -> (String, Int) -> Bool
monosame (m, i) (n, j) = m == n

gather :: [[(String, Int)]] -> [(String, Int)]
gather = map (\m -> (fst (head m), sum (map snd m)))

monoshow :: (String, Int) -> String
monoshow (s, n) | n > 1 = '+' : (show n ++ s)
                | n == 1 = '+' : s
                | n == 0 = ""
                | n == -1 = '-' : s
                | n < -1 = show n ++ s

display :: [(String, Int)] -> String
display = dropWhile (== '+') . foldl (\s m -> s ++ monoshow m) ""

simplify :: String -> String
simplify = display . gather . groupBy monosame . sortBy monocmp . polyParse
