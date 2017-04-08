import Data.Vect

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do s <- getLine
                       ss <- readVectLen k
                       pure (s :: ss)

-- Длина была известна. А что делать, если длина сначала не известна?

readVect : IO (len ** Vect len String) -- ** - зависимая пара
readVect = do s <-getLine 
              if s == ""
                 then pure (_ ** []) -- можно писать (0 ** _)
                 else do (k ** ss) <- readVect
                         pure ((S k) ** s :: ss)

myfilter : (a -> Bool) -> Vect n a -> (k ** Vect k a)
myfilter f [] = (0 ** [])
myfilter f (x :: xs )
  = let (_ ** xs') = myfilter f xs in
        if f x then (_ ** x :: xs')
               else (_ ** xs')
