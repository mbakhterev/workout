(import (ice-9 match)
	(rnrs base)
	(rnrs arithmetic fixnums (6)))

(define (plus e1 e2) (fx+ e1 e2))

(define minus (case-lambda ((e1) (fx- e1))
			   ((e1 e2) (fx+ e1 e2))))

(define (int n)
  (if (fixnum? n) n (error 'eval "not a fixnum" n)))

(define ())

