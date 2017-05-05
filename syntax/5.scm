(define (stat-files p size)
  (let loop ((max-size 0) (file "")
             (max-length 0) (name "")
             (max-elements 0) (path "")
             (max-literal-len 0) (literal "")
             (total 0)
             (meta-size size)
             (n-files 0)
             (total-elements 0)
             (total-elements-len 0))
    (let ((str (read-line p)))
      (if (eof-object? str)
        (list n-files
              max-size file
              max-length name
              max-elements path
              max-literal-len literal
              total
              meta-size
              (ceiling (/ total-elements-len total-elements)))
        (let* ((l (string-split str #\space))
               (file-size (locale-string->integer (car l)))
               (file-path (cadr l))
               (len (string-length file-path))
               (literals (string-split file-path #\/))
               (n-elements (length literals))
               (lit (car (sort literals (lambda (x y) (> (string-length x) (string-length y))))))
               (lit-len (string-length lit)))
          (loop (max max-size file-size)
                (if (> file-size max-size) file-path file)
                (max max-length len)
                (if (> len max-length) file-path name)
                (max max-elements n-elements)
                (if (> n-elements max-elements) file-path path)
                (max max-literal-len lit-len)
                (if (> lit-len max-literal-len) lit literal)
                (+ total file-size)
                (+ 1 len meta-size)
                (+ 1 n-files)
                (+ total-elements n-elements)
                (fold + total-elements-len (map string-length literals))))))))

(define (stat-files p size)
  (let loop
       (lambda max-size file
               max-length name
               max-elements path
               max-literal-len literal
               total
               meta-size
               n-files
               total-elements
               total-elements-len
         (let str (read-line p)
           (if (eof-object? str)
             (tuple n-files
                    max-size file
                    max-elements path
                    max-literal-len literal
                    total
                    meta-size
                    (ceiling (/ total-elements-len total-elements)))
             (let l (string-split str \space)
                  file-size (locale->string->integer (head l))
                  file-path (tail l)
                  len (string-length file-path)
                  literals (string-split file-path #\/)
                  n-elements (length literals)
                  lit (head (sort literals (lambda x y (> (string-length x) (string-length y)))))
                  lit-len (string-length lit)
               (loop (max max-size file-size)
                     (if (> file-size max-size) file-path file)
                     (max max-length len)
                     (if (> len max-length) file-path name)
                     (max max-elements n-elements)
                     (if (> n-elements max-elements) file-path path)
                     (max max-literal-len lit-len)
                     (if (> lit-len max-literal-len) lit literal)
                     (+ total file-size)
                     (+ 1 len meta-size)
                     (+ 1 n-files)
                     (+ total-elements n-elements)
                     (fold + total-elements-len (map string-length literals)))))))
       (loop 0 ""
             0 ""
             0
             size
             0
             0
             0)))

(define (stat-files p size)
  (let ((loop
         (lambda (max-size file
                  max-length name
                  max-elements path
                  max-literal-len litera
                  total
                  meta-size size
                  n-files
                  total-elements
                  total-elements-len)
           (let ((str (read-line p)))
             (if (eof-object? str)
               (tuple n-files
                      max-size file
                      max-length name
                      max-elements path
                      max-literal-len literal
                      total
                      meta-size
                      (ceiling (/ total-elements-len total-elements)))
               (let ((l (string-split str #\space))
                     (file-size (locale-string->integer (car l)))
                     (file-path (cadr l))
                     (len (string-length file-path))
                     (literals (string-split file-path #\/))
                     (n-elements (length literals))
                     (lit (car (sort literals (lambda (x y) (> (string-length x) (string-length y))))))
                     (lit-len (string-length lit)))
                 (loop (max max-size file-size)
                       (if (> file-size max-size) file-path file)
                       (max max-length len)
                       (if (> len max-length) file-path name)
                       (max max-elements n-elements)
                       (if (> n-elements max-elements) file-path path)
                       (max max-literal-len lit-len)
                       (if (> lit-len max-literal-len) lit literal)
                       (+ total file-size)
                       (+ 1 len meta-size)
                       (+ 1 n-files)
                       (+ total-elements n-elements)
                       (fold + total-elements-len (map string-length literals)))))))))
    (loop 0 ""
          0 ""
          0 ""
          0
          0 0
          0
          0
          0)))
