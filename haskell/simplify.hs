import Data.Char
import Data.List
import qualified Data.Map as M
import Debug.Trace

newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Functor Parser
  where
    fmap f p = Parser (\s -> map (\(v, r) -> (f v, r)) (parse p s))

instance Applicative Parser
  where
    pure v = Parser (\s -> [(v, s)])
    pf <*> p = Parser (\s -> do (f, r) <- parse pf s 
                                (x, t) <- parse p r
                                return (f x, t))

instance Monad Parser
  where
    return v = Parser (\s -> [(v, s)])
    p >>= k = Parser (\s -> parse p s >>= (\(v, r) -> parse (k v) r))

zero :: Parser a
zero = Parser (const [])

item :: Parser Char
item = Parser (\s -> case s of { [] -> []; (a : s) -> [(a, s)] })

match :: Parser a -> (a -> Bool) -> Parser a
match m p = m >>= \t -> if p t then return t else zero

digit :: Parser Char
digit = match item isDigit

variable :: Parser Char
variable = match item isAlpha

oneof :: Parser a -> Parser a -> Parser a
oneof m n = Parser (\s -> parse m s ++ parse n s)

choice :: Parser a -> Parser a -> Parser a
choice m n = Parser (\s -> let p = (parse m s) in if null p
                                                  then parse n s
                                                  else p)
reiterate :: Show a => Parser a -> Parser [a]
reiterate m = choice multiple (return [])
  where
    multiple = do t <- m
                  ts <- reiterate m
                  return (t : ts)

number :: Parser Int
number = reiterate digit >>= \ds -> return (read ds :: Int)

monom :: Parser String
monom = reiterate variable

sign :: Parser Char
sign = choice (match item (== '+')) (match item (== '-'))

factor :: Parser Int
factor = choice signed number
  where
    signed = do s <- sign
                n <- number
                return (case s of { '-' -> (- n); '+' -> n })

data Term = Monom Int String deriving (Show)

polyParse :: String -> Char
polyParse = fst . head . parse sign

-- split :: String -> [(String, Integer)]
-- split = collect . tokenize "+|[0-9]*|[a-z]+"
--   where
--     collect = id

simplify :: String -> String
simplify = const "hello world" 
