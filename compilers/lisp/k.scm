(import (ice-9 match))

(define atom? (compose not pair?))

(define evaluate
  (match-lambda
    ((? symbol? e) (evaluate-variable e))
    ((or (? number? e)
	 (? null? e)) (evaluate-quote e))
    (('quote e) (evaluate-quote e))
    (('if ec et ef) (evaluate-if ec et ef))
    (('begin . e*) (evaluate-begin e*))
    (('lambda v* . e*) (evaluate-lambda v* e*))
    ((e . e*) (evaluate-applicaton e e*))
    (e (error "неизвестный синтаксис" e))))
