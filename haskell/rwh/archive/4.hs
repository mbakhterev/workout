import System.Environment (getArgs)

interactwith function infile outfile
  = do input <- readFile infile
       writeFile outfile (function input)

main = mainwith fn where
  mainwith function
    = do args <- getArgs
         case args of
           [inf, outf] -> interactwith function inf outf
           _ -> putStrLn "error: 2 arguments needed"
  fn = id

splitLines [] = []
splitLines cs =
  
