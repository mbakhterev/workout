import Debug.Trace

newtype Product = Product Integer deriving (Show)
newtype Factor = Factor Integer deriving (Show)

quotrem :: Product -> Factor -> (Integer, Integer)
quotrem (Product n) (Factor p) = quotRem n p

factorout :: Factor -> Product -> (Integer, Product)
factorout p n = let (q, r) = quotrem n p
                in if r /= 0
                   then (0, n)
                   else let (d, m) = factorout p (Product q)
                        in (d + 1, m)

overfactor :: Factor -> Product -> Bool
overfactor (Factor p) (Product m) = p > m

bumpfactor :: Factor -> Factor
bumpfactor (Factor p) = Factor (p + 1)

maxfactor :: Integer -> (Integer, Factor)
maxfactor n = loop (1, Factor n) (Factor 2) (Product n) where
  loop f p m = if overfactor p m
               then f
               else let (k, Product q) = factorout p m
                    in if k == 0
                       then loop f (bumpfactor p) m 
                       else if q == 1
                            then (k, p)
                            else loop (k, p) (bumpfactor p) (Product q)

countfactors :: Factor -> Integer -> Integer
countfactors (Factor p) n = if n == 0
                            then 0
                            else let q = quot n p
                                 in q + countfactors (Factor p) q

showbn b n = "b: " ++ show b ++ " n: " ++ show n

zeroes :: Integral a => a -> a -> a
zeroes b n = let (k, Factor p) = maxfactor (toInteger (trace (showbn (toInteger b)  (toInteger n)) b))
                 m = countfactors (Factor p) (toInteger n)
             in fromInteger (quot m k)
