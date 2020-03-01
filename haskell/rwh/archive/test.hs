newtype X = Int
newtype Y = Int

test1 :: X -> Y -> Int
test1 = (+)

test2 :: (X -> Y -> Int) -> Int -> Int -> Int
test2 f a b = f b a

