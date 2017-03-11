data BookInfo
  = Book Int String [String] deriving (Show)

data MagazineInfo
  = Magazine Int String [String] deriving (Show)

myinfo = Book 12345 "The Art of UNIX Programming" ["RMS"]

type CustomerID = Int
type ReviewBody = String

data BookReview = BookReview CustomerID ReviewBody

fntest :: (Int -> String -> BookReview) -> CustomerID -> ReviewBody -> BookReview
fntest f a b = f a b

type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo
  = CreditCard CardNumber CardHolder Address
  | CashOnDelivery
  | Invoice CustomerID
  deriving Show

data Catacean = Catacean String String
data Furniture = Furniture String String

data CartesianD 
  = Cartesian2D Double Double
  deriving (Eq, Show)

data Polar2D
  = Polar2D Double Double
  deriving (Eq, Show)

data Roygbiv
  = Red | Orange | Yellow | Green | Blue | Violet

data Shape
  = Circle Vector Double
  | Poly [Vector]

type Vector = (Double, Double)

notme True = False
notme x = True

sumlist (x:xs) = x + sumlist xs
sumlist z = 0

third (a, b, c) = b

complicated (True, a, x:xs, 5) = (a, xs)

bookId (Book id title authors) = id
bookAuthors (Book id title authors) = authors
