(define (min-max-one w p) (let ((m (car p))
                                (M (cdr p))
                                (l (length w)))
                            (cons (min m l)
                                  (max m l))))

(define (min-max-all s)
  (foldl min-max-one
         (let ((l (length (car s)))) (cons l l))
         (cdr s)))

(define (max-dif-length s1 s2)
  (if (or (null? s1)
          (null? s2))
    -1
    (let ((mm-1 (min-max-all s1))
          (mm-2 (min-max-all s2)))
      (max (abs (- (car mm-1) (cdr mm-2)))
           (abs (- (cdr mm-1) (cdr mm-2)))))))
