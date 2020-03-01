split_with :: (a -> Bool) -> [a] -> [[a]]

split_with p [] = []
split_with p l
  = let oks = takeWhile (not .p) l
        rst = dropWhile p (dropWhile (not . p) l)
        in oks : split_with p rst
                    
