import Data.Bits

data M2 = M2 Integer Integer Integer Integer deriving (Show)

data V2 = V2 Integer Integer deriving (Show)

mmul :: M2 -> M2 -> M2
mmul (M2 a00 a01 a10 a11) (M2 b00 b01 b10 b11) =
  M2 (a00 * b00 + a01 * b10) -- r00
     (a00 * b10 + a01 * b11) -- r01
     (a10 * b00 + a11 * b10) -- r10
     (a10 * b10 + a11 * b11) -- r11

vmul :: M2 -> V2 -> V2
vmul (M2 a00 a01 a10 a11) (V2 b0 b1) =
  V2 (a00 * b0 + a01 * b1) -- r0
     (a10 * b0 + a11 * b1) -- r1

mpos = M2 0 1 1 1
mneg = M2 (-1) 1 1 0

mone = (M2 1 0 0 1)

mexpt :: M2 -> Integer -> M2
mexpt m n = loop mone m n
  where
    loop r m 0 = r
    loop r m n | (n .&. 1) == 1 = loop (mmul r m) (mmul m m) (shiftR n 1)
               | otherwise = loop r (mmul m m) (shiftR n 1)

fib :: Integer -> Integer
fib n | n >= 0 = (\(V2 a b) -> a) (vmul (mexpt mpos n) (V2 0 1))
      | otherwise = (\(V2 a b) -> a) (vmul (mexpt mneg (- n)) (V2 0 1))
