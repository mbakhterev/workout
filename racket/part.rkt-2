(define num car)
(define mul cdr)
(define summand cons)

; Добавляем одно слагаемое в сумму. Добавляется число, не превосходящее уже
; существующее в сумме. Слагаемые в сумме упорядочены по возрастанию
(define (bump-one v P)
  (let ((n (car P)))
    (if (= v (num n))
        (cons (summand v (+ 1 (mul n))) (cdr P))
        (cons (summand v 1) P))))

; Добавка единицы в первое слагаемое: убрать одно слагаемое, добавить 1 число в
; следующее слагаемое. Если следующих нет, то ничего не делаем, это учтено в
; других вариантах
(define (bump-rest P)
  (let ((n (car P))
        (ns (cdr P)))
    (cond ((empty? ns) (list))

          ((= 1 (mul n)) (cons (bump-one (+ 1 (num n)) ns)
                               (map (lambda (p) (cons n p)) (bump-rest ns))))

          (else (cons (cons (summand (num n) (- (mul n) 1))
                            (bump-one (+ 1 (num n)) ns))
                      (map (lambda (p) (cons n p)) (bump-rest ns)))))))

; Все способы добавить 1 к разбиению P. 
(define (bump P) (cons (bump-one 1 P) (bump-rest P)))

(define (part N)
  (let loop ((n 0) (P '()))
    (if (= n N)
        P
        (loop (+ 1 n)
              (append (append* (map bump P))
                      (list (list (summand (+ 1 n) 1))))))))

(define (part->vector P)
  (let loop ((p P) (V #()))
    (if (empty? p)
      V
      (loop (cdr p) (vector-append V (make-vector (mul (car p)) (num (car p))))))))
