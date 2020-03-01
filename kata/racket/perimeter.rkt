(define (fibs a b) (stream-cons a (fibs b (+ a b))))

(define (perimeter n)
  (stream-fold + 0 (stream-take
                     (stream-map (lambda (x) (* 4 x)) (fibs 1 1))
                     (+ n 1))))
