import Control.Applicative (Alternative (..))
import Control.Monad.Trans.State.Strict
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as M

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
                     (return (Mono (es * s * n) Nothing)))
           (factored (es * s)) -- если множитель отсутствует, то factored-часть обязательна

poly :: Parser [Mono]
poly = tokenize ((:) <$> mono 1 <*> polychain)
       where polychain = (<|>) (sign >>= \s -> (:) <$> mono s <*> polychain)
                               (return [])

binding :: Parser (Char, [Mono])
binding = flip (,) <$> poly <*> (tokenize (match item (== '=')) *> variable)

flatten :: [Mono] -> [(Integer, Maybe Char)]
flatten l = l >>= \m -> case m of
                         Mono f v -> return (f, v)
                         Factor f p -> flatten p >>= \(n, w) -> return (n * f, w)

collect :: [(Integer, Maybe Char)] -> [(Integer, Maybe Char)]
collect = map reduce . groupBy samevar . sortOn snd
  where
    samevar (m, v) (n, u) = v == u
    reduce l = (sum (map fst l), snd (head l))

polymerize :: [Mono] -> [Mono]
polymerize = map (\ (n, v) -> Mono n v) . collect . flatten
  
environize :: [String] -> M.Map Char [Mono]
environize = M.fromList . map (\((v, p), r) -> (v, polymerize p))
                        . catMaybes
                        . map (parse binding) 

substitute :: M.Map Char [Mono] -> [(Integer, Maybe Char)] -> [Mono]
substitute env = map substone
  where
    substone (f, Nothing) = Mono f Nothing
    substone (f, Just v) = case M.lookup v env of Just m -> Factor f m; Nothing -> Mono f (Just v)

isChanged :: [Mono] -> Bool
isChanged = any isfactor
  where
    isfactor Mono f v = False
    isfactor Factor f p = True

simplify :: [String] -> String -> String
simplify es s = ""
