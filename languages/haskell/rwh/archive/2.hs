dropme n xs = if n < 1 || null xs
    	        then xs
    	        else dropme (n - 1) (tail xs)

onebutlast []  = Nothing
onebutlast [x] = Nothing
onebutlast xs  = Just . head . tail . reverse $ xs

pass [] = \_ -> []
pass xs = flip ($) xs
dropit 0 = id
dropit n = flip pass $ dropit (n - 1) . tail
