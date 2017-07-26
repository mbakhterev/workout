(let loop ((i (read))) (if (not (= i 42)) (begin (display i) (newline) (loop (read)))))
