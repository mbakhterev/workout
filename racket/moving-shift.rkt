(define (shift-char c n)
  (define (biased-shift bias char n)
    (let ((b (char->integer bias))
          (c (char->integer char)))
      (integer->char (+ b (modulo (+ n c (- b)) 26)))))

  (cond ((not (char-alphabetic? c)) c)
        ((char-upper-case? c) (biased-shift #\A c n))
        (else (biased-shift #\a c n))))

(define (step i)
  (lambda (c p)
    (cons (+ i (car p))
          (string-append (cdr p) (string (shift-char c (car p)))))))

(define (encypher s shift)
  (cdr (foldl (step 1) (cons shift "") (string->list s))))

(define (decypher s shift)
  (cdr (foldl (step -1) (cons (- shift) "") (string->list s))))

(define (chunks l)
  (let ((m (quotient l 5)))
    (if (= (* 5 m) l)
        (list m m m m m)
        (let loop ((n 5)
                   (o (- l (* 4 m)))
                   (R '()))
          (cond ((= 1 n) (cons o R))
                ((zero? o) (loop (- n 1) 0 (cons m R)))
                (else (loop (- n 1) (- o 1) (cons (+ 1 m) R))))))))

(define (chunk-string s)
  ((compose cdr foldl)
   (lambda (n p) (let ((end (car p))
                       (start (- (car p) n)))
                   (cons start
                         (cons (substring s start end) (cdr p)))))
   (cons (string-length s) '())
   (chunks (string-length s))))

(define (moving-shift s shift) (chunk-string (encypher s shift)))

(define (demoving-shift s shift) (decypher (apply string-append s) shift))
