(define (n-make-generators n)
  (let ((v (make-vector n)))
    (do ((i 0 (+ i 1)))
        ((= i n) v)
        (vector-set! v i (/ 1 (sqr (+ 2 i)))))))

(define (vector-sum V)
  (do ((n (- (vector-length V) 1) (- n 1))
       (sum 0.0 (+ sum (exact->inexact (vector-ref V n)))))
      ((negative? n) sum)))

(define (n-sums k n)
  (let* ((G (n-make-generators n))
         (V (vector-copy G)))
    (let loop ((i 1) (S '()))
      (if (> i k)
          S
          (let ((s (/ (vector-sum V) i)))
            (vector-map! * V G)
            (loop (+ i 1) (cons s S)))))))

(define (double-boxes k n) (foldl + 0.0 (n-sums k n)))



