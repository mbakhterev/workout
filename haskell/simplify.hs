import Control.Applicative (Alternative (..))
import Control.Monad.Trans.State.Strict
import Data.Char

newtype Parser a = Parser { current :: StateT String Maybe a }
parse = runStateT . current
wrap = Parser . StateT

instance Functor Parser where
  fmap f p = Parser (fmap f (current p))

instance Applicative Parser where
  pure = Parser . pure
  fp <*> p = Parser (current fp <*> current p)

instance Alternative Parser where
  empty = Parser empty
  p <|> q = Parser (current p <|> current q)

instance Monad Parser where
  m >>= f = wrap (\s -> do (v, t) <- parse m s; parse (f v) t)

data Mono = Mono Integer (Maybe Char)
          | Factor Integer [Mono]
          deriving (Show)

match :: Parser a -> (a -> Bool) -> Parser a
match m p = m >>= \t -> if p t then return t else empty

item :: Parser Char
item = wrap (\s -> case s of [] -> empty; (c:cs) -> return (c,cs))

space :: Parser ()
space = many (match item isSpace) >>= \t -> return ()

tokenize :: Parser a -> Parser a
tokenize p = p <* space

number :: Parser Integer
number = tokenize (some (match item isDigit)) >>= \t -> return (read t :: Integer)

variable :: Parser Char
variable = tokenize (match item isAlpha)

sign :: Parser Integer
sign = do s <- tokenize (match item (\c -> (c == '+') || (c == '-')))
          return (case s of '+' -> 1; '-' -> -1)

parenfy :: Parser a -> Parser a
parenfy p = (tokenize (match item (== '('))) *> p <* (tokenize (match item (== ')')))

-- Часть слагаемого с обязательной переменной или подвыражением в скобках.
-- Параметризовано множителем m

factored :: Integer -> Parser Mono
factored m = (<|>) (variable >>= \v -> return (Mono m (Just v)))
                   (parenfy poly >>= \p -> return (Factor m p))

-- es - это знак, передаваемый из внешнего выражения
mono :: Integer -> Parser Mono
mono es =
  do s <- sign <|> pure 1 -- знак может быть или не быть. Это ни на что не влияет
     (<|>) (do n <- number -- если присутствует множитель, то factored-части может и не быть
               (<|>) (factored (es * s * n))
                     (return (Mono (es * s *n) Nothing)))
           (factored s) -- если множитель отсутствует, то factored-часть обязательна

-- mono :: Integer -> Parser Mono
-- mono es =
--   do s <- sign <|> pure 1
--      n <- number <|> pure 1
--      (variable >>= \v -> return (Mono (es * s * n) (Just v)))
--       <|> (parenfy poly >>= \p -> return (Factor (es * s * n) p))
--       <|> (return (Mono (es * s * n) Nothing))

poly :: Parser [Mono]
poly =
  tokenize
    (((:) <$> mono 1 <*> poly)
      <|> (sign >>= \s -> (:) <$> mono s <*> poly)
      <|> return [])

-- E  -> T E'
-- E' -> + T E' | -TE' |epsilon
-- T  -> F T'
-- T' -> * F T' | /FT' |epsilon
-- F  -> (E) | int

simplify :: [String] -> String -> String
simplify es s = ""
