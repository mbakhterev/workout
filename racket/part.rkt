(define (combine N K) (for*/set ((n N) (k K)) (* n k)))

(define (parts N)
  (let ((P (make-vector (+ 1 N) (set)))
        (limit (lambda (n) (+ 1 (quotient (+ (remainder n 2) n) 2)))))
    (for ((n (in-range 1 (+ 1 N))))
      (vector-set! P n (for/fold ((R (set n)))
                                 ((k (in-range 1 (limit n))))
                                 (set-union R (combine (vector-ref P k)
                                                       (vector-ref P (- n k)))))))
    (list->vector (sort (set->list (vector-ref P N)) <))))

(define (params P)
  (let ((N (vector-length P)))
    (values (- (vector-ref P (- N 1)) 1)
            (/ (foldl + 0 (vector->list P)) N)
            (let ((n-half (quotient N 2)))
              (if (odd? N)
                (vector-ref P n-half)
                (/ (+ (vector-ref P n-half)
                      (vector-ref P (- n-half 1)))
                   2))))))

(define (part n)
  (let-values (((r a m) (params (parts n))))
    (format "Range: ~a Average: ~a Median: ~a"
            r
            (~r a #:precision '(= 2))
            (~r m #:precision '(= 2)))))
