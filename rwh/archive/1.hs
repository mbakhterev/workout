fns = [length . lines, length . words, length]
main = interact $ \str -> show $ map (flip ($) str) fns
