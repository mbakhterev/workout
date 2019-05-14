(define (bezout-gcd a b)
  (define (step r-2 s t r-1 u v)
    (if (zero? r-1)
        (values r-2 s t)
        (let-values (((q r) (quotient/remainder r-2 r-1)))
          (step r-1 u v
                r (- s (* q u)) (- t (* q v)))))) 
  (step a 1 0 b 0 1))

(define (z-reverse n p)
  (let-values (((r u v) (bezout-gcd p n)))
    (and (eqv? 1 r)
         (remainder v p))))

(define (solve-chinese c . cs)
  (define rem car)
  (define mod cdr)

  (let ((M (foldl * 1 (map mod (cons c cs)))))
    (let loop ((x 0)
               (C (cons c cs)))
      (if (empty? C)
          x
          (let* ((r (rem (car C)))
                 (m (mod (car C)))
                 (k (quotient M m))
                 (k-reverse (z-reverse k m)))
            (if (not (number? k-reverse))
                #f
                (loop (+ x (* r k k-reverse)) (cdr C))))))))

(define base (solve-chinese '(1 . 9) '(2 . 7)))
(define cycle (* 7 9))

(define (first-f n)
  (if (< base n)
      (let ((r (remainder (- n base) cycle)))
        (if (zero? r)
            n
            (+ n cycle (- (remainder (- n base) cycle)))))
      (let ((r (remainder (- base n) cycle)))
        (if (zero? r)
            n
            (+ n (remainder (- base n) cycle))))))

(define (last-f n)
  (if (<= base n)
      (let ((r (remainder (- n base) cycle)))
        (- n r))
      (let ((r (remainder (- base n) cycle)))
        (- n (- cycle r)))))

(define (present f)
  (list (format "M: ~a" f)
        (format "B: ~a" (quotient (- f 2) 7))
        (format "C: ~a" (quotient (- f 1) 9))))

(define (how-much n m)
  (let ((l (min n m))
        (r (max n m)))
    (do ((f (last-f r) (- f cycle))
         (answer '() (cons (present f) answer)))
        ((< f l) answer))))
