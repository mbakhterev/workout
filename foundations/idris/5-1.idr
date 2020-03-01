import Data.Vect

readVect : IO (len ** Vect len String) -- ** - зависимая пара
readVect = do s <-getLine 
              if s == ""
                 then pure (_ ** []) -- можно писать (0 ** _)
                 else do (k ** ss) <- readVect
                         pure ((S k) ** s :: ss)

zipInputs : IO ()
zipInputs = do (l1 ** v1) <- readVect
               (l2 ** v2) <- readVect
               case exactLength l1 v2 of
                    Nothing => putStrLn "Ошибка"
                    (Just x) => printLn (zip v1 x)
