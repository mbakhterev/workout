(import (ice-9 match))

(define atom? (compose not pair?))
(define (with-k v) (λ (r) (λ (k) (k v))))

(define (evaluate e)
  (cond ((or (null? e)
	     (number? e)
	     (eq? 'true e)) (evaluate-quote e))
	((symbol? e) (evaluate-variable e))
	((not (pair? e)) (error "неизвестный синтаксис" e))
	(else (case (car e)
		((quote) (evaluate-quote (cadr e)))
		((fix) (evaluate-fix (cadr e) (caddr e)))
		((if) (evaluate-if (cadr e) (caddr e) (cadddr e)))
		((begin) (evaluate-begin (cdr e)))
		((lambda) (evaluate-lambda (cadr e) (cddr e)))
		(else (evaluate-applicaton (car e) (cdr e)))))))

;; (define (evaluate-quote v) (with-k v))

(define evaluate-quote with-k)

(define (evaluate-if ec et ef)
  (λ (r) (((evaluate ec) r) (if-cont et ef r))))

(define (if-cont et ef r)
  (λ (v) ((evaluate (if (null? v) ef et)) r)))

(((evaluate '(if (if () () 3) 1 'a)) '()) identity)

(define (evaluate-begin e*)
  (cond ((atom? e*) (with-k '()))
	((atom? (cdr e*)) (evaluate (car e*)))
	(else (λ (r) (((evaluate (car e*)) r) (begin-cont e* r))))))

(define (begin-cont e* r)
  (λ (v) ((evaluate-begin (cdr e*)) r)))

(define (empty-env n)
  (λ (k) (error "в окружении нет переменной" n)))

(define (top-eval e) (((evaluate e) empty-env) identity))

(top-eval '(begin
	     (begin 'a 'b 1)
	     (begin)
	     4
	     (if (begin)
		 (begin 1)
		 (begin 'a))))

(define (bind-env n v r)
  (λ (n-requested) (if (eq? n-requested n)
		       (λ (k) (k v))
		       (r n-requested))))
