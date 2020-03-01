import Data.Vect

rotate : Vect n a -> Vect n a
rotate [] = []
rotate (x :: xs) = insLast x xs
  where
    insLast : a -> Vect k a -> Vect (S k) a
    insLast x [] = [x]
    insLast x (y :: xs) = y :: insLast x xs

-- plus len 1 посчитать нельзя, потому что информации о len нет никакой.
-- Можно написать доказательство.
-- Надо переписать реализацию

-- data Fin : Nat -> Type where
--   FZ : Fin (S k)
--   FS : Fin k -> Fin (S k)

-- Конечные множества. Fin 3 - это FZ, FS FS, FS (FS FZ)

x : Fin 10
x = FZ

v : Vect 5 Nat
v = [1, 2, 4, 5, 6]

v3 : Nat
v3 = index 3 v

-- v6 : Nat
-- v6 = index 6 v

-- {n = n} направление связывания n =: n

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n = n} i xs = case integerToFin i n of
                          Nothing => Nothing
                          (Just i') => Just (index i' xs)

record Person where
  constructor MkPerson
  firstName, middleName, lastName : String
  age : Int

fred : Person
fred = MkPerson "Fred" "Joe" "Bloggs" 30

record SizedClass (size : Nat) where
  constructor SizedClassInfo
  students : Vect size Person
  className : string

-- addStudent : Person -> SizedClass n -> SizedClass (S n)
-- addStudent p c = record { students = p :: students c } c

main : IO ()
main = putStrLn "Привет, мир"
