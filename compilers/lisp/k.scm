(define atom? (compose not pair?))

;; попытка сделать в стиле такой семантике
;; (((evaluate expression) continuation) environment)

(define (with-kr e) (λ (k) (λ (r) ((e k) r))))
(define (with-k e) (λ (k) (k e)))

(define (evaluate e)
  (if (atom? e)
      (cond ((symbol? e) (evaluate-variable e))
	    (else (evaluate-quote e)))
      (case (car e)
	((quote) (evaluate-quote (cadr e)))
	((if) (evaluate-if (cadr e) (caddr e) (cadddr e)))
	((begin) (evaluate-begin (cdr e)))
	((set!) (evaluate-set! (cadr e) (caddr e)))
	((lambda) (evaluate-lambda (cadr e) (cddr e)))
	(else (evaluate-applicaton (car e) (cdr e))))))

(define (evaluate-quote v) (with-k v))

(define (evaluate-if ec et ef)
  (λ (k) ((evaluate ec) (if-cont et ef k))))

(define (if-cont et ef k)
  (λ (v) ((evaluate (if (null? v) ef et)) k)))

(define (top-cont v) (λ (r) v))

(((evaluate '(if (if () () 3) 1 'a)) top-cont) '())

(define (evaluate-begin e*)
  (cond ((not (pair? e*)) (with-k '()))
	((not (pair? (cdr e*))) (evaluate (car e*)))
	(else (λ (k)
		((evaluate (car e*)) (begin-cont e* k))))))

(define (begin-cont e* k)
  (λ (v) ((evaluate-begin (cdr e*)) k)))

(define (top-eval e) (((evaluate e) top-cont) top-env))

(define top-env '())

(define (test) (top-eval '(begin (begin 'a 'b 1) (begin) 4 (if (begin) (begin 1) (begin 'a)))))

