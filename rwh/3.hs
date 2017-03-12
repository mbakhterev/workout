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
complicated _ = (0, [])

bookId (Book id title authors) = id
bookAuthors (Book id title authors) = authors

data Customer
  = Customer { customerId :: CustomerID
             , customerName :: String
             , customerAddress :: Address }
             deriving (Show)

customer1 = Customer 1 "2" ["3", "4", "5"]

cuatomer2 = Customer { customerName = "6"
                     , customerAddress = ["7", "8"]
                     , customerId = 1 }

wrapped = Just (Just "it")

data List a = Nil | Cons a (List a) deriving (Show)

tolist (x:xs) = Cons x (tolist xs)
tolist [] = Nil

fromlist Nil = []
fromlist (Cons x xs) = x : (fromlist xs)

data Tree a
  = Node a (Tree a) (Tree a) | Empty
    deriving (Show)

data TreeToo a
  = NodeToo a (Maybe (TreeToo a)) (Maybe (TreeToo a))
    deriving (Show)

secondof a = if null (tail a)
             then error "too short"
             else head (tail a)

secondon [] = Nothing
secondon [x] = Nothing
secondon (x:y:z) = Just y

lend amount balance
  = let reserve = 100
        newbalance = balance - amount
     in if balance < reserve
        then Nothing
        else Just newbalance

foo = let a = 10 in let b = 30 in a + b

bar = let x = "1" in ((let x = 1 in x), x)

quux a = let a = "foo" in a ++"eek!"

lendwhere amount balance
  = if balance < reserve
    then Nothing
    else Just newbalance
  where reserve = 100
        newbalance = balance - amount

pluralise :: String -> [Int] -> [String]
pluralise word = map plural
  where plural 0 = "0 " ++ word ++ "s"
        plural 1 = "1 " ++ word
        plural n = show n ++ " " ++ word ++ "s"

xfoo = let a = 1
           b = 2
           c = 3
       in a + b + c

xbar = let { a = 4; b = 5
           ; c = 9 }
       in
           a + b + c

maybeval defval v
  = case v of
      Nothing -> defval
      Just x -> x

samenodes (Node a _ _) (Node b _ _)
  | a == b = Just a

samenodes n m = Nothing
