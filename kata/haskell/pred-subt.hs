{-# LANGUAGE RankNTypes #-}

newtype Number = Nr (forall a. (a -> a) -> a -> a)

zero :: Number
zero = Nr (\ f x -> x)

succ :: Number -> Number
succ (Nr n) = Nr (\f x -> f (n f x))

pred :: Number -> Number
pred (Nr n) = Nr (\f x -> n (\g h -> h (g f)) (const x) id)

subt :: Number -> Number -> Number
subt n (Nr m) = (m Main.pred) n
