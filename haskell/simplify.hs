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

variable :: Parser (Maybe Char)
variable = tokenize ((match item isAlpha) >>= Just 

sign :: Parser Integer
sign = do s <- tokenize (match item (\c -> (c == '+') || (c == '-')))
          case s of '+' -> 1; '-' -> -1

mono :: Parser Mono
mono = do s <- sign <|> pure 1
          n <- number <|> pure 1
          do v <- variable <|> ; 
            

poly :: Parser [Mono]

-- E  -> T E'
-- E' -> + T E' | -TE' |epsilon
-- T  -> F T'
-- T' -> * F T' | /FT' |epsilon
-- F  -> (E) | int

simplify :: [String] -> String -> String
simplify es s = ""
