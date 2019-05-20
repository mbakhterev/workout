import Data.Char
import Control.Applicative (Alternative (..))

-- String -> Maybe (a, String) = StateT Maybe String a, для которого реализованы
-- разные классы. Можно ими пользоваться. Но для обучения имеет смысл сделать
-- всё вручную
newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  -- Логика fmap: из имеющейся в p функции parse, создаём новую функцию,
  -- которая сделает то, что делает (parse p), но получаемое при разборе
  -- значение пропускает через f. Вроде как, это разумно и выполняет все
  -- требования к fmap.
  fmap f p = Parser (\s -> parse p s >>= \(v, r) -> return (f v, r)) 

instance Applicative Parser where
  pure v = Parser (\s -> Just (v, s))

  -- Логика <*>: нужно извлечь из fp функцию (как бы она туда ни попала бы), по
  -- идее (несколько безумной) эта функция должна попасть в fp после некоторого
  -- разбора исходной строки. И эту функцию мы должны применить к значению из
  -- оставшегося разбора строки. Очень странная логика, но другая не приходит в
  -- голову
  fp <*> p = Parser (\s -> do (f, r) <- parse fp s
                              (v, t) <- parse p r
                              return (f v, t))
    
instance Alternative Parser where
  empty = Parser (\s -> Nothing)
  -- Логика <|>: нужно получить результат разбора p и результат разбора q, и
  -- воспользоваться <|> для Maybe
  p <|> q = Parser (\s -> parse p s <|> parse q s)

instance Monad Parser where
  -- Логика m >>= f: надо извлечь результат из m, передать его через >>= в Maybe
  -- в f. Вроде как, очевидно.
  m >>= f = Parser (\s -> parse m s >>= \(v, r) -> parse (f v) r)

polyParse :: String -> Maybe a
polyParse s = case parse empty s of { Just (v, s) -> Just v; Nothing -> Nothing }

item :: Parser Char
item = Parser (\s -> case s of { [] -> empty; (c : cs) -> pure (c, cs) })

match :: Parser a -> (a -> Bool) -> Parser a
match m p = m >>= \t -> if p t then return t else empty

digit :: Parser Char
digit = match item isDigit

number :: Parser Int
number = some digit >>= \ds -> return (read ds :: Int)

variable :: Parser Char
variable = match item isAlpha

monom :: Parser String
monom = some variable

-- parse (some number) "12345"
