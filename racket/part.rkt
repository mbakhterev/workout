(define num car)
(define mul cdr)
(define summand cons)

(define (part-merge K N)
  (cond ((empty? K) N)
        ((empty? N) K)
        (else (let ((n (car N))
                    (k (cdr N)))
                (cond ((< (num n) (num k)) (cons n (parts-merge K (cdr N))))
                      ((< (num k) (num n)) (cons k (parts-merge (cdr K) N)))
                      (else (cons (summand (num k) (+ (mul k) (mul n)))
                                  (post-merge (cdr K) (cdr N)))))))))

(define (part-merge-all K N) (for*/list ((k K) (n N)) (part-merge k n)))

(define (part n)
  (let ((P (make-vector (+ n 1) '())))
    (for ((m (in-range 1 ))))
    
    )
  )
