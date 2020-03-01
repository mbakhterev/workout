import Data.List

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

fnthree n
  | n <= 10 = 20 * n
  | n >  10 = 40 * n

icedrop n xs | n <= 0 = xs
icedrop _ []          = []
icedrop n (_:xs)      = icedrop (n-1) xs

lengther :: [a] -> Int
lengther = foldl (+) 0 . map (const 1)

average :: Fractional t => [t] -> t
average = snd . foldl cma (0.0, 0.0)
  where cma (n, s) x = (n+1.0, (x + n*s) / (n+1.0))

palindromize l = l ++ reverse l

ispalindrome t = t == reverse t

lensort :: [[a]] -> [[a]]
lensort = sortBy (flip (compare . length) . length)

interpose a = tail . foldl ((++) . flip (++) [a]) []

theight (Node x a b) = 1 + max (theight a) (theight b)
theight Empty = 0

data Direction
  = ToLeft | ToRight | Straight
    deriving (Show, Eq)

data Point2D = P2D Double Double deriving (Show)
data Vector2D = V2D Double Double deriving (Show)

vec (P2D a b) (P2D x y) = V2D (x-a) (y-b)
len (V2D a b) = sqrt (a*a + b*b)
dot (V2D a b) (V2D x y) = a*x + b*y
cross (V2D a b) (V2D x y) = a*y - b*x
crosspoints a b c = cross (vec a b) (vec b c)

cosvec a b
  | len a /= 0 && len b /= 0 = dot a b / (len a * len b)
  | otherwise = -1

direction a b c
  | crosspoints a b c > 0 = ToLeft
  | crosspoints a b c < 0 = ToRight
  | otherwise = Straight
 
directions (a:b:c:ps)
  = direction a b c : directions (b:c:ps)
directions ps = []

testpoints = [(P2D 0 20),
              (P2D 1 3),
              (P2D 4 5),
              (P2D 6 7),
              (P2D 11 45),
              (P2D 7 9),
              (P2D 100 (- 13))]

minpoint :: [Point2D] -> Point2D
minpoint
  = minimumBy cmp
  where cmp (P2D a b) (P2D x y) = compare (b, a) (y, x)

sortpoints p
  = sortBy cmp
  where val a = - cosvec (vec p a) (V2D 1 0)
        cmp a b = compare (val a) (val b)

leftpush (b:a:stack) p
  | direction a b p == ToRight = leftpush (a:stack) p
  | otherwise = p:b:a:stack
leftpush cs p = p:cs

grahamscan points
  = foldl leftpush [p] sorted
  where p = minpoint points
        sorted = sortpoints p points
