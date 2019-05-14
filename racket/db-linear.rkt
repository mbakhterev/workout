; Пока не получается выявить полезную закономерность в числах, поэтому решение
; задачи при помощи биномиальной кучи.

; Структура узлов деревьев в куче
(struct node (rank value forest))

; Связывание двух деревьев ранга r в дерево ранга r + 1.
(define (link t1 t2)
  (let ((r (node-rank t1))
        (v1 (node-value t1))
        (v2 (node-value t2)))
    (if (<= v1 v2)
      (node (+ 1 r) v1 (cons t2 (node-forest t1)))
      (node (+ 1 r) v2 (cons t1 (node-forest t2))))))

(define (insert-tree H t)
;  (display H) (newline)
  (if (empty? H)
      (list t)
      (let ((ht (car H)))
        (if (< (node-rank t) (node-rank ht))
            (cons t H)
            (insert-tree (cdr H) (link t ht))))))

(define (insert heap value) (insert-tree heap (node 0 value '())))

(define (insert-all heap vs)
  (foldl (lambda (v h) (insert h v)) heap vs))

(define (merge h1 h2)
  (cond ((empty? h1) h2)
        ((empty? h2) h1)
        (else (let* ((t1 (car h1))
                     (t2 (car h2))
                     (r1 (node-rank t1))
                     (r2 (node-rank t2)))
                (cond ((< r1 r2) (cons t1 (merge (cdr h1) h2)))
                      ((< r2 r1) (cons t2 (merge h1 (cdr h2))))
                      (else (insert-tree (merge (cdr h1) (cdr h2)) (link t1 t2))))))))

(define (extract-min heap)
  (define (extract-min-tree H)
    (let ((t (car H))
          (ts (cdr H)))
      (if (empty? ts)
        (values t '())
        (let-values (((m ms) (extract-min-tree ts)))
          (if (< (node-value t) (node-value m))
            (values t ts)
            (values m (cons t ms)))))))
  
  (let-values (((t ts) (extract-min-tree heap)))
    (values (node-value t) (merge (node-forest t) ts))))

(define (next-min current-min heap)
  (let loop ((m current-min)
             (H heap))
    (let-values (((n hn) (extract-min H)))
      (if (not (= m n))
          (values n hn)
          (loop n hn)))))

(define (test)
  (let loop ((m -1)
             (h (insert-all '() '(10 20 10 30 40 1 4 1 12 4 5 0 1 1 0 10))))
    (when (not (empty? h))
      (let-values (((v hn) (next-min m h)))
;        (display v)
;        (newline)
        (loop v hn)))))

(define (db-linear N) 
  (let loop ((n N) (m 1) (H '()))
    (if (zero? n)
        m
        (let-values (((v h) (next-min m (insert (insert H (+ 1 (* 2 m)))
                                                (+ 1 (* 3 m))))))
          (loop (- n 1) v h)))))
