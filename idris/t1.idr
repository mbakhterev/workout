import Data.Vect

insert : (x : Nat) -> (xs : Vect len Nat) -> Vect (S len) Nat
insert x [] = [x]
insert x ys@(y :: xs) = case x < y of
                             False => y :: insert x xs
                             True => x :: ys

inssort : Vect n Nat -> Vect n Nat
inssort [] = []
inssort (x :: xs)
  = let xs' = inssort xs in (insert x xs')
