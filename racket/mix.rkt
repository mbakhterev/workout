(define (extract str)
  (filter (lambda (p) (not (= 1 (car p))))
          (map (lambda (l) (cons (length l) (car l)))
               (group-by identity
                         (sort (filter (lambda (c) (and (char-alphabetic? c)
                                                        (char-lower-case? c)))
                                       (string->list str))
                               char<?)))))

(define (merge ps qs)
  (cond ((and (empty? ps) (empty? qs)) '())

        ((empty? qs) (cons (cons 1 (car ps)) (merge (cdr ps) '())))
        ((empty? ps) (cons (cons 2 (car qs)) (merge '() (cdr qs))))

        ((char=? (cdar ps) (cdar qs))
         (cons (cond ((= (caar ps) (caar qs)) (cons 3 (car ps)))
                     ((> (caar ps) (caar qs)) (cons 1 (car ps)))
                     (else (cons 2 (car qs))))
               (merge (cdr ps) (cdr qs))))

        ((char<? (cdar ps) (cdar qs)) (cons (cons 1 (car ps)) (merge (cdr ps) qs)))
        (else (cons (cons 2 (car qs)) (merge ps (cdr qs))))))

(define (item<? p q) (if (not (= (cadr p) (cadr q)))
                         (> (cadr p) (cadr q))
                         (if (not (= (car p) (car q))) 
                             (< (car p) (car q))
                             (char<? (cddr p) (cddr q)))))

(define (encode i)
  (let ((n (car i))
        (p (cdr i)))
    (string-append (if (= 3 n) "=" (number->string n))
                   ":"
                   (list->string (build-list (car p) (const (cdr p)))))))

(define (mix s t)
  (string-join (map encode (sort (merge (extract s) (extract t)) item<?))
               "/"))
