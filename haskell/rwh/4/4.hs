import System.Environment (getArgs)

interactwith function infile outfile
  = do input <- readFile infile
       writeFile outfile (function input)

oldmain = mainwith fn where
  mainwith function
    = do args <- getArgs
         case args of
           [inf, outf] -> interactwith function inf outf
           _ -> putStrLn "error: 2 arguments needed"
  fn = id

isLineEnd c = c == '\r' || c == '\n'

splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
  let (pre, suffix) = break isLineEnd cs in
    pre : case suffix of
            ('\r' : '\n' : rest) -> splitLines rest
            ('\r' : rest) -> splitLines rest
            ('\n' : rest) -> splitLines rest
            _ -> []

main = mainwith fn where
  mainwith function
    = do args <- getArgs
         case args of
           [inf, outf] -> interactwith function inf outf
           _ -> putStrLn "error: 2 arguments needed"
  fn = unlines . splitLines
