safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:xs) = Just x

safetail :: [a] -> Maybe [a]
safetail [] = Nothing
safetail (x:xs) = Just xs

safelast :: [a] -> Maybe a
safelast [] = Nothing
safelast xs = Just (last xs)

safeinit :: [a] -> Maybe [a]
safeinit [] = Nothing
safeinit xs = Just (init xs)
