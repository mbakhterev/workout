-- data Term = Con Int | Div Term Term
--             deriving (Show)
-- 
-- parseTerm :: String -> Term
-- parseTerm = fst . head . parse term
-- 
-- term :: Parser Term
-- term = do
--     t <- factor
--     term' t
-- 
-- term' :: Term -> Parser Term
-- term' t = divFactor `bchoice` return t
--     where divFactor = do
--             lit '/'
--             u <- factor
--             term' $ Div t u
-- 
-- factor :: Parser Term
-- factor = numTerm `choice` parenTerm
--     where numTerm = do
--             n <- number
--             return $ Con n
--           parenTerm = do
--             lit '('
--             t <- term
--             lit ')'
--             return t
-- 
-- lit :: Char -> Parser Char
-- lit c = item `filt` \c' -> c == c'
-- 
-- reiterate :: Parser a -> Parser [a]
-- reiterate m = multiple `bchoice` return []
--     where multiple = do
--             t <- m
--             ts <- reiterate m
--             return $ t : ts
-- 
-- number :: Parser Int
-- number = do
--     ds <- reiterate digit
--     return (read ds :: Int)
-- 
-- main = print $ parseTerm "1972/2/23"
-- 

import Data.Char
import Data.Map
import Data.List

newtype Parser a = Parser { parse :: String -> [(a, String)]

instance Monad Parser
  where
    return v = Parser (\s -> [(v, s)])
    p >>= k = Parser (\s -> parse p s >>= (\(v, r) -> parse (k v) r))

zero :: Parser a
zero = Parser (const [])

item :: Parser Char
item = Parser (\s -> case s of { [] -> []; (a : s) -> [(a, s)] }

match :: Parser a -> (a -> Bool) -> Parser a
match m p = m >>= \c -> if p c then m else zero

digit :: Parser Char
digit = match item isDigit

variable :: Parser Char
variable = match item isAlpha

oneof :: Parser a -> Parser a -> Parser a
oneof m n = Parser (\s -> parse m s ++ parse n s)

choice :: Parser a -> Parser a -> Parser a
choice m n = Parser (\s -> let p = (parse m s) in if p == []
                                                  then parse n s
                                                  else p)

reiterate :: Parser a -> Parser [a]
reiterate m = choice multiple (return [])
  where
    multiple = do t <- m
                  ts <- reiterate m
                  return (t : ts)


data Term = Factor Int String (deriving Show)

polyParse :: String -> Term
polyParse = fst . head . parse term


digit 

-- split :: String -> [(String, Integer)]
-- split = collect . tokenize "+|[0-9]*|[a-z]+"
--   where
--     collect = id

simplify :: String -> String
simplify = const "hello world" 
