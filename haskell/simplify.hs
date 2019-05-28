import Control.Applicative (Alternative (..))
import Control.Monad.Trans.State.Strict
import Data.Char

newtype Parser a = Parser { current :: StateT String Maybe a }
parse = runStateT . current

instance Functor Parser where
  fmap f p = Parser (fmap f (current p))

instance Applicative Parser where
  pure = Parser . pure
  fp <*> p = Parser (current fp <*> current p)

instance Alternative Parser where
  empty = Parser empty
  p <|> q = Parser (current p <|> current q)

instance Monad Parser where
  m >>= f = (Parser . StateT) (\s -> do (v, t) <- parse m s
                                        parse (f v) t)

simplify :: [String] -> String -> String
simplify es s = ""
