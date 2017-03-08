data BookInfo
  = Book Int String [String] deriving (Show)

data MagazineInfo
  = Magazine Int String [String] deriving (Show)

myinfo = Book 12345 "The Art of UNIX Programming" ["RMS"]

