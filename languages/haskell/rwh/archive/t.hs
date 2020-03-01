import Data.List

lensort :: Foldable t => t a -> t a
lensort = sortBy (flip (compare . length) . length)

data Angle = Double deriving (Num)
