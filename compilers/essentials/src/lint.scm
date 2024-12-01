(import (ice-9 match)
	(rnrs base)
	(rnrs arithmetic fixnums (6)))

(define (int value) (if (fixnum? value)
			(list 'int value)
			(error 'int "should be fixnum:" value)))

(define (prim op . args) (if (member op '(read + -))
			     (cons 'prim (cons op args))
			     (error 'prim "not a primitive op:" op)))

;; Все подвыражения (read) выглядят одинаково, поэтому можно не создавать всякий
;; раз cons-ячейку под них. Хорошо бы было, чтобы компилятор сам умел до такого
;; додумываться (вероятно, он умеет это делать), но на всякий случай.

(define parse-expr
  (let ((rd (prim 'read)))
    (match-lambda
      ((? fixnum? n) (int n))
      (('- e1) (prim '- (parse-expr e1)))
      (('- e1 e2) (prim '- (parse-expr e1) (parse-expr e2)))
      (('+ e1 e2) (prim '+ (parse-expr e1) (parse-expr e2)))
      (('read) rd))))

(define (parse-program expr) (program '() (parse-expr expr)))

(define (program info body) (list 'program info body))

; (define eight (int 8))
; (define neg-eight (p:- eight))

(define ast1.1 (parse-expr '(+ (read) (- 8))))

(define leaf
  (match-lambda
    ((or ('int _)
	 ('prim 'read)) #t)
    (('prim '- e1) #f)
    (('prim '- e1 e2) #f)
    (('prim '+ e1 e2) #f)))

(leaf ast1.1)
;; (leaf eight)

(define expr?
  (match-lambda
    (('int n) (fixnum? n))
    (('prim 'read) #t)
    (('prim '- e1) (expr? e1))
    ((or ('prim '- e1 e2)
	 ('prim '+ e1 e2)) (and (expr? e1) (expr? e2)))
    (else #f)))

(define Lint?
  (match-lambda
    (('program '() body) (expr? body))
    (else #f)))

(program '() ast1.1)
(Lint? (program '() ast1.1))

(define eval-expr
  (match-lambda
    (('int n) (if (fixnum? n)
		    n
		    (error 'eval-expr "int should be fixnum" n)))
    (('prim 'read) (let ((n (read)))
		     (if (fixnum? n)
			 n
			 (error 'eval-expr "read expects and integer" n))))
    (('prim '- e1) (fx- (eval-expr e1)))
    (('prim '+ e1 e2) (let* ((v1 (eval-expr e1))
			     (v2 (eval-expr e2)))
			(fx+ v1 v2)))
    (('prim '- e1 e2) (let* ((v1 (eval-expr e1))
			     (v2 (eval-expr e2)))
			(fx- v1 v2)))))

(expr? ast1.1)

(define eval-Lint
  (match-lambda
    (('program '() expr) (eval-expr expr))))

'(eval-expr ast1.1)


(eval-Lint
 (program '() (p:+ (int 10) (p:- (p:+ (int 12) (int 20))))))

(define pe-neg
  (match-lambda
    (('int n) (int (fx- n)))
    (r (p:- r))))

(define pe-add
  (match-lambda*
    ((('int n1) ('int n2)) (int (fx+ n1 n2)))
    ((e1 e2) (p:+ e1 e2))))

(define pe-sub
  (match-lambda*
    ((('int n1) ('int n2)) (int (fx- n1 n2)))
    ((e1 e2) (p:- e1 e2))))

(define pe-expr
  (match-lambda
    (('int n) (int n))
    (('prim 'read) rd)
    (('prim '- e1) (pe-neg (pe-expr e1)))
    (('prim '- e1 e2) (pe-sub (pe-expr e1) (pe-expr e2)))
    (('prim '+ e1 e2) (pe-add (pe-expr e1) (pe-expr e2)))))

(define pe-Lint
  (match-lambda
    (('program '() expr) (program '() (pe-expr expr)))))

(define p1 (program '() (p:+ (int 10) (p:- (p:+ (int 12) (int 20))))))

;; (pe-Lint p1)

(define p2 (parse-program '(+ 10 (- (+ 5 3)))))
(define p3 (parse-program '(+ 1 (+ 3 1))))
(define p4 (parse-program '(- (+ 3 (- 5)))))

(define (test-pe p)
  (assert (equal? (eval-Lint p) (eval-Lint (pe-Lint p)))))

(test-pe p2)
(test-pe p3)
(test-pe p4)
