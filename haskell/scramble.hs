import Data.List (group, sort)

scramble :: [Char] -> [Char] -> Bool
scramble s1 s2 = check (group (sort s1)) (group (sort s2))
  where
    check ps [] = True
    check [] qs = False
    check (p:ps) (q:qs) =
      if (head p == head q) && (length p >= length q)
      then check ps qs
      else if (head p < head q)
           then check ps (q:qs)
           else False
