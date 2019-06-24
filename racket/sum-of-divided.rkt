(define (prime-out p N)
  (if (= 1 (abs p))
      1
      (let-values (((q r) (quotient/remainder N p)))
        (if (zero? r) (prime-out p q) N))))

; Тут идея должна быть в том, что перебор следует начинать с максимального, уже
; известно простого числа в P. По идее, если их просто cons-ить, то так и будет.
; Но это не сработает, потому что могут быть пропуски в делителях. Ну, надо
; убрать все известные делители, и начать перебор от 2.

(define (factorize N P)
  (let loop ((r (foldl prime-out (abs N) P))
             (p 2)
             (R P))
    (cond  
      ; Вытащили всё, что можно
      ((= 1 (abs r)) R)
      ; Что осталось, то простое число, если по модулю не равно 1.
      ((> p r) (cons p R))
      (else (let ((r-next (prime-out p r)))
              (if (= r r-next)
                  ; Если не поделилось, то p и не делитель
                  (loop r (+ 1 p) R)
                  ; Если поделилось, то p простой делитель
                  (loop r-next (+ 1 p) (cons p R)))))))) 

(define (sum-of-divided lst) #f)
