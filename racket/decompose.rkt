(define (next-limit N k)
  (let ((l (integer-sqrt (- N (* k k))))) (min (- k 1) l)))

(define (search N k)
  (if (<= k 0)
      #f
      (let ((R (- N (* k k))))
        (if (zero? R)
            (list k)
            (let ((p (search R (next-limit N k))))
              (if (list? p)
                  (cons k p)
                  (search N (- k 1))))))))

(define (decompose n)
  (let ((r (search (* n n) (- n 1))))
    (if (list? r) (reverse r) '())))
