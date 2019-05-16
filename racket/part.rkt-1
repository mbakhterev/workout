(define num car)
(define mul cdr)
(define summand cons)

(define (bump-one v P)
  (if (empty? P)
      (cons (summand v 1) '())
      (let ((n (car P)))
        (if (= v (num n))
            (cons (summand v (+ 1 (mul n))) (cdr P))
            (cons (summand v 1) P)))))

(define (bump-rest P)
  (if (empty? P)
    '()
    (let ((n (car P))
          (ns (cdr P)))
      (cons (if (= 1 (mul n))
                (bump-one (+ 1 (num n)) ns)
                (cons (summand (num n) (- (mul n) 1))
                      (bump-one (+ 1 (num n)) ns)))
            (map (lambda (p) (cons n p)) (bump-rest ns))))))

(define (bump P) (cons (bump-one 1 P) (bump-rest P)))

(define (part N)
  (if (zero? N)
      (set)
      (let loop ((n 1) (P (set '((1 . 1)))))
        (if (= n N)
            P
            (loop (+ 1 n)
                  (foldl (lambda (p S) (set-union S (list->set p)))
                         (set)
                         (set-map P bump)))))))

(define (prod-one p)
  (foldl (lambda (s v) (* v (expt (num s) (mul s))))
         1
         p))

(define (prod P)
  (list->set (set-map P prod-one)))
