(define (prime-out-one p N)
  (if (= 1 (abs p))
      N
      (let-values (((q r) (quotient/remainder N p)))
        (if (zero? r) (prime-out-one p q) N))))

; P -- список пар (простое . сумма) 
(define (primes-out N P)
  (let ((result (foldl (lambda (p R)
                         (let ((prime (car p))
                               (sum (cdr p))
                               (n (car R))
                               (l (cdr R)))
                           (let ((r (prime-out-one prime n)))
                             (if (= r n)
                                 ; prime не является делителем N
                                 (cons n (cons p l))
                                 ; prime является делителем, N нужно добавить в
                                 ; сумму
                                 (cons r (cons (cons prime (+ sum N)) l))))))
                      (cons N '())
                      P)))
    (values (car result) (cdr result))))

(define (factorize N P)
  (let-values (((n prime-pairs) (primes-out N P)))
    (let loop ((r (abs n))
               (p 2)
               (R prime-pairs))
      (cond  
        ; Вытащили всё, что можно
        ((= 1 (abs r)) R)
        ; Что осталось, то простое число, и его надо добавить в список сумм
        ((> p r) (cons (cons p N) R))
        (else (let ((r-next (prime-out-one p r)))
                (if (= r r-next)
                    ; Если не поделилось, то p и не делитель
                    (loop r (+ 1 p) R)
                    ; Если поделилось, то p простой делитель
                    (loop r-next (+ 1 p) (cons (cons p N) R))))))))) 

(define (sum-of-divided lst)
  (map (lambda (p) (list (car p) (cdr p)))
       (sort (foldl factorize '() lst)
             (lambda (a b) (< (car a) (car b))))))
