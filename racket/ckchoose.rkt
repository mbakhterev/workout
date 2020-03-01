(define (checkchoose m n)
  (cond ((zero? m) -1)
        ((equal? 1 m) 0)
        (else
          (let loop ((k 1)
                     (current-n n)
                     (mk-factorial m)
                     (upper-n n))
            (display (format "k: ~a, mk!: ~a, n!/(n-k)!: ~a\n"
                             k
                             mk-factorial
                             upper-n))
            (cond ((equal? mk-factorial upper-n) k)
                  ((> (* 2 k) n) -1)
                  (else 
                    (loop (+ 1 k)
                          (- current-n 1)
                          (* mk-factorial (+ 1 k))
                          (* upper-n (- n 1)))))))))
