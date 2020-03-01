prev : Nat -> Nat 
prev Z = 0
prev (S k) = k

plus' : Nat -> Nat -> Nat
plus' Z j = j
plus' (S k) j = S (plus' k j)

mutual
  odd : Nat -> Bool
  odd Z = False
  odd (S k) = even k

  even: Nat -> Bool
  even Z = True
  even (S k) = odd k
