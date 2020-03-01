import Data.Vect

insert : Ord elem => (x : elem) -> (xs : Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x ys@(y :: xs) = case x < y of
                             False => y :: insert x xs
                             True => x :: ys

inssort : Ord elem => Vect n elem -> Vect n elem
inssort [] = []
inssort (x :: xs)
  = let xs' = inssort xs in (insert x xs')

vhead : Vect (1 + n) elem -> elem
vhead (x :: xs) = x

vhead' : (n: Nat) -> (elem: Type) -> Vect (1 + n) elem -> elem
vhead' n elem (x :: xs) = x

zeroes : Vect n Nat 
zeroes {n = Z} = []
zeroes {n = (S _)} = 0 :: zeroes

v: Vect 10 Nat
v = zeroes
