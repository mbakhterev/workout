fibonacci :: Int -> Integer
fibonacci = (map fib [0 ..] !!)
  where fib 0 = 1
        fib 1 = 1
        fib n = fibonacci (n - 1) + fibonacci (n - 2)
