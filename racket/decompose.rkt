(define (next-limit N k)
  (let ((l (integer-sqrt (- N (* k k))))) (min (- k 1) l)))

(define (search N k)
  (let ((R (- N (* k k))))
    (if (zero? R)
      (list k)
      (let loop ((l (next-limit N k)))
        (if (<= l 0) 
            #f
            (let ((p (search R l)))
              (if (list? p)
                  (cons k p)
                  (loop (- l 1)))))))))

(define (decompose n) #f)
