(define (freq<? p q) (if (= (car p) (car q))
                         (char<? (cdr p) (cdr q))
                         (> (car p) (car q))))

(define (extract str)
  (filter (lambda (p) (not (= 1 (car p))))
          (map (lambda (l) (cons (length l) (car l)))
               (group-by identity
                         (sort (filter (lambda (c) (and (char-alphabetic? c)
                                                        (char-lower-case? c)))
                                       (string->list str))
                               char<?)))))

(define (item<? p q) (if (not (= (cadr p) (cadr q)))
                         (> (cadr p) (cadr q))
                         (if (and (zero? (car p)) (zero? (car q)))
                             (char<? (cddr p) (cddr q))
                             (zero? (car q)))))


(define (encode n p)
  (string-append (if (zero? n) "=" (number->string n))
                 ":"
                 (list->string (build-list (car p) (const (cdr p))))))

(define (merge ps qs)
  (cond ((and (empty? ps) (empty? qs)) '())
        ((empty? qs) (cons (cons 1 (car ps)) (merge (cdr ps) '())))
        ((empty? ps) (cons (cons 2 (car qs)) (merge '() (cdr qs))))

        ((char=? (cdar ps) (cdar qs))
         (cons (cond ((= (caar ps) (caar qs)) (cons 0 (car ps)))
                     ((> (caar ps) (caar qs)) (cons 1 (car ps)))
                     (else (cons 2 (car qs))))
               (merge (cdr ps) (cdr qs))))

        ((freq<? (car ps) (car qs)) (cons (cons 1 (car ps)) (merge (cdr ps) qs)))
        (else (cons (cons 2 (car qs)) (merge ps (cdr qs))))))

(define (mix s t)
  (string-join (map (lambda (x) (encode (car x) (cdr x)))
                    (sort (merge (extract s) (extract t)) item<?))
               "/"))
