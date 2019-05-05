go :: [Int] -> [Int] -> [Int] -> Int -> [Int]
go (i:is) (j:js) (k:ks) 1 = []
go (i:is) (j:js) (k:ks) n =
  let vi = 2 * i
      vj = 3 * j
      vk = 5 * k
      m  = min vi (min vj vk)
      ni = if m == vi then is else i : is
      nj = if m == vj then js else j : js
      nk = if m == vk then ks else k : ks
  in m : go ni nj nk (n - 1)

hamming n = let s = 1 : go s s s n in last s
