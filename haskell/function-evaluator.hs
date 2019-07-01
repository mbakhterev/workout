import qualified Data.Map.Strict as M

factorial i | i == 0    = Left 1
            | otherwise = Right ([i-1], (*i).head)

evaluateFunction :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f = \n -> case f n of
                             Left m -> m
                             Right (ns, g) -> g (map (evaluateFunction f) ns)


