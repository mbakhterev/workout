(define atom? (compose not pair?))

;; попытка сделать в стиле такой семантики
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

(define (test) (top-eval '(begin
			    (begin 'a 'b 1)
			    (begin)
			    4
			    (if (begin)
				(begin 1)
				(begin 'a)))))

(define (empty-env n k)
  (error "в окружении нет переменной:" n))

(define (bind-env n v r)
  (λ (n-requested k) (if (eq? n-requested n)
			   (k v)
			   (r n-requested k))))

(define (evaluate-variable n)
  (λ (k) (λ (r) ((r n k) r))))

(define (make-env b*)
  (cond ((null? b*) empty-env)
	((and (symbol? (car b*))
	      (pair? (cdr b*)))
	 (bind-env (car b*)
		     (cadr b*)
		     (make-env (cddr b*))))
	(else (error "некоррректное окружение:"
		     b*))))

(define (extend-env n* v* r)
  (cond ((and (null? n*) (null? v*)) r)
	((and (symbol? n*)) (bind-env n* v* r))
	((and (pair? n*)
	      (pair? v*)
	      (symbol? (car n*)))
	 (bind-env (car n*)
		   (car v*)
		   (extend-env (cdr n*) (cdr v*) r)))
	(else (error "несовпадение арностей:" n* v*))))

(define (with-env e b*)
  (((evaluate e) top-cont) (make-env b*)))

(with-env '(if a b c) '(a () b 2 c 3))

(define (closure r n* e*)
  (λ (v*)
    (λ (k)
      (((evaluate-begin e*) k) (extend-env n*
					   v*
					   r)))))

(define (evaluate-lambda n* e*)
  (λ (k) (λ (r) (k (closure r n* e*)))))

(define (evaluate-applicatonx e e*)
  (λ (k)
    (λ (r)
      (((evaluate e) (fn-cont e* k r)) r))))

(define (fn-cont e* k r)
  (λ (f)
    (((evaluate-arguments e*) (app-cont f k r)) r)))

(define (app-cont f k)
  (λ (v*) ((f v*) k)))

(define (evaluate-arguments e*)
  (if (pair? e*)
      (λ (k) (λ (r) (у((evaluate (car e*)) (arguments-cont e* k)) r)))
      (λ (k))))
