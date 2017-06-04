(let distributed-matrix-mul
     (lambda mat-r mat-a mat-b
             (for (lambda i j k (run matrix-mul (zip 'i.j) (pin mat-a 'i.k) (pin mat-b 'k.j)))
                  (range l)
                  (range n)
                  (range m))

             (let queue-sum (lambda result sum-ref blocks-ref n-blocks
                                    (if (zero? n-blocks)
                                        (run let result (pin sum-ref))
                                        (begin (run matrix-add (! pin :sum) (pin sum-ref) (zip blocks-ref))
                                               (run queue-sum result (? pin sum) blocks-ref n-blocks)))))

             (for (lambda i j (run queue-sum (! pin mat-r 'i.j) (zero-matrix) (? zip 'i.j)))
                  (range l)
                  (range m))))

(let watchdog
     (lambda zproc start
             (if (not (= zproc :done)) (run watchdog zproc start)
                                       (print "time:" (- (current-time) start))))
     
     main
     (lambda args
             (run watchdog (zip :proc) (current-time))

             (run fork (? zip :proc)
                       distributed-matrix-mul (! pin (nth args 1) (span))
                                              (? pin root (nth args 2))
                                              (? pin root (nth args 3)))
             
             (run let (! zip api :update-root) (? pin))))

