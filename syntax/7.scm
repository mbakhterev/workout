(def gauss-random (sigma (o mu 0))
     "gausian distributed random with width sigma around mu"
     (withs (u (rand)
               v (* 1.7156 (- (rand) 0.5))
               x (- u 0.449871)
               y (+ abs.v 0.386595)
               q (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x)))))
            (while (and (> q 0.27597)
                        (or (> q 0.27846) (> (* v v) (* -4 log.u u u))))
                   (= u (rand)
                      v (* 1.7156 (- (rand) 0.5))
                      x (- u 0.449871)
                      y (+ abs.v 0.386595)
                      q (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x))))))
            (+ mu (/ (* sigma v) u))))

(let gauss-random
     (oneof (lambda (sigma) (gauss-random sigma 0.0))
            (lambda (sigma mu)
              (loop it (let u (rand)
                            v (* 1.7156 (- (rand) 0.5))
                            x (- u 0.449871)
                            y (+ (abs v) 0.386595)
                            q (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x)))))
                    (if (and (> q 0.27597)
                             (or (> q 0.27846) (> (* v v) (* -4 log.u u u))))
                      (it )
                      (+ mu (/ (* sigma v)) u))))))

Да уж. Фиговенько.

(let gauss-random
     (oneof (lambda (sigma) (gauss-random sigma 0.0))
            (lambda (sigma mu)
                    (loop (step))
                    (let step (lambda u v () (let u (rand))
                                                  v (* 1.7156 (- (rand) 0.5)))
                         loop (lambda (u v)
                                      (let x (- u 0.449871)
                                           y (+ (abs v) 0.386595)
                                           q (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x)))))
                                      (if (and (> q 0.27597)
                                               (or (> q 0.27846) (> (* v v) (* -4 (log u) u u))))
                                          (loop (step))
                                          (+ mu (/ (* sigma v) u))))))))

(def gauss-random (sigma (o mu 0))
     "gausian distributed random with width sigma around mu"
     (withs (u (rand)
               v (* 1.7156 (- (rand) 0.5))
               x (- u 0.449871)
               y (+ abs.v 0.386595)
               q (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x)))))
            (while (and (> q 0.27597)
                        (or (> q 0.27846) (> (* v v) (* -4 log.u u u))))
                   (= u (rand)
                      v (* 1.7156 (- (rand) 0.5))
                      x (- u 0.449871)
                      y (+ abs.v 0.386595)
                      q (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x))))))
            (+ mu (/ (* sigma v) u))))

(let [[wait-trace guide] (loop [t [L]] (if-let [g (deref G 100 nil)] [t g] (recur (approximate-move (->Control 0 4) t))))])

(let wait-trace guide (let loop (lambda t g (trace) (let dg (deref G 100 nil) (if dg (let t trace g dg) (loop (approximate-move (Control 0 4) t)))))))

(let ((tuple wait-trace guide) (let loop (t (vector L)) (let g (deref G 100 nil) (if g (tuple t g) (loop (approximate-move (Control 0 4) t)))))))

(bind gauss-random (oneof (lambda (sigma) (gauss-random sigma 0.0))
                          (lambda (sigma mu)
                                  (let loop (u (rand)
                                             v (* 1.7156 (- (rand) 0.5)))
                                    (let (x (- u 0.449871)
                                          y (+ (abs v) 0.386595)
                                          q (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x)))))
                                      (if (and (> q 0.27597)
                                               (or (> q 0.27846) (> (* v v) (* -4 (log u) u u)))) ))))))


(let wait-trace guide (loop t) (let loop (lambda t g (trace) ())))

(let block-n 64
     l 100
     n 200
     m 300)

(let distributed-matrix-mul
     (lambda mat-r (mat-a mat-b)
             (for-each (lambda (i j k) (run (zip 'i.j) ((pin mat-a 'i.k) (pin mat-b 'k.j)) matrix-mul))
                       (range l)
                       (range n)
                       (range m))

             (let queue-sum (lambda result (sum block-queue n-blocks)
                                    (if (= 0 n-blocks)
                                        (run result (sum) let)
                                        (do (run (pin :sum) ((pin sum (zip block-queue))) matrix-add)
                                            (run result ((ref pin :sum) (ref zip block-queue) (- n-blocks 1)) queue-sum)))))

             (for-each (lambda (i j) (run (pin mat-r 'i.j) ((zero-matrix) (? zip 'i.j)) queue-sum))
                       (range m)
                       (range l))))

(fn watchdog (zproc start)
  (if (not= zproc :done) (run watchdog zproc start)
                         (print "time:" (- (current-time) start))))

(fn main (args)
  (run watchdog (zip "proc") (current-time))

  (fork (? zip "proc")
        distributed-matrix-mul (! pin (nth args 1) (span ""))
                               (? pin root (seq (nth args 2) (nth args 3))))

  (run (fn (r s) (:= r s)) (! zip api :update-root) (? pin)))

(let distributed-matrix-mul
     (lambda mat-r mat-a mat-b
             (for (lambda i j k (run matrix-mul (zip 'i.j) (pin mat-a 'i.k) (pin mat-b 'k.j)))
                  (range l)
                  (range n)
                  (range m))

             (let queue-sum (lambda result sum-ref blocks-ref n-blocks
                                    (if (zero? n-blocks)
                                        (run let result (pin sum-ref))
                                        (run matrix-add (! pin :sum) (pin sum-ref) (zip blocks-ref)
                                             queue-sum result (? pin sum) blocks-ref n-blocks))))

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

(def gauss-random (sigma (o mu 0))
     "gausian distributed random with width sigma around mu"
     (withs (u (rand)
               v (* 1.7156 (- (rand) 0.5))
               x (- u 0.449871)
               y (+ abs.v 0.386595)
               q (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x)))))
            (while (and (> q 0.27597)
                        (or (> q 0.27846) (> (* v v) (* -4 log.u u u))))
                   (= u (rand)
                      v (* 1.7156 (- (rand) 0.5))
                      x (- u 0.449871)
                      y (+ abs.v 0.386595)
                      q (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x))))))
            (+ mu (/ (* sigma v) u))))

(let gauss-random
     (oneof (lambda sigma (gauss-random sigma 0))
            (lambda sigma mu
                    (let step (lambda (tuple (rand)
                                             (* 1.7156 (- (rand) 0.5))))
                         loop (lambda (tuple u v)
                                      (let x (- u 0.449871)
                                           y (+ (abs v) 0.386595)
                                           q (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x)))))
                                      (if (and (> q 0.27597)
                                               (or (> q 0.27846) (> (* v v) (* -4 (log u) u u))))
                                          (loop (step))
                                          (+ mu (/ (* sigma v) u)))))
                    (loop (step)))))

(let wait-trace guide (let loop (lambda t g (trace) (let dg (deref G 100 nil) (if dg (let t trace g dg) (loop (approximate-move (Control 0 4) t)))))))

(let (tuple wait-trace guide) (begin (let L (read-lander)
                                          S (read-surface)
                                          G (future (make-guide L S))
                                          loop (lambda t (let g (deref G 100 nil))
                                                         (if g (tuple t g)
                                                               (loop (approximate-move (Control 0 4 t))))))
                                     (loop (vector L))))

(let ((tuple wait-trace guide) (let (L (read-lander)
                                     S (read-surface)
                                     G (future (make-guide L S))
                                     loop (lambda (t) (let (g (deref G 100 nil))
                                                           (if g (tuple t g) (loop (approximate-move (Control 0 4) t))))))
                                    (loop (vector L)))))

(let (tuple wait-trace guide) (begin (let G (begin (let L (read-lander)
                                                        S (read-surface))
                                                   (future (make-guide L S))))
                                     (let loop (lambda t (let g (deref G 100 nil))
                                                         (if g (tuple t g) (loop (approximate-move (Control 0 4) t)))))
                                     (loop (vector L))))

(let ((tuple wait-trace guide) (let (L (read-lander) 
                                     G (let (S (read-surface)) (future (make-guide L S))))
                                    (let loop (t (vector L)) (let (g (deref G 100 nil))
                                                                  (if g (tuple t g) (loop (approximate-move (Control 0 4) t))))))))


(let [[wait-trace guide] (loop [t [L]] (if-let [g (deref G 100 nil)] [t g] (recur (approximate-move (->Control 0 4) t))))])

(let (tuple wait-trace guide) (begin (let L (read-lander)
                                          G (begin (let S (read-surface)) (future (make-guide L S))))
                                     (let loop (lambda t (let g (deref G 100 nil))
                                                         (if g (tuple t g) (loop (approximate-move (Control 0 4) t)))))
                                     (loop (vector L))))

(let (tuple wait-trace guide)
     (begin (let L (read-lander)
                 G (begin (let S (read-surface)) (future (make-guide L S)))
                 loop (lambda t (let g (deref G 100 nil))
                                (if g (tuple t g) (loop (approximate-move (Control 0 4) t)))))
            (loop (vector L))))

(let approximate-move
     (lambda (type Control control) (type Vector trace) 
             (let l (move control 1.0 (last trace)))
             (append trace (assoc l :x (round (:x l))
                                    :y (round (:y l))
                                    :vx (round (:vx l))
                                    :vy (round (:vy l))))))

(bind approximate-move
      (lambda ((type Control control) (type Vector trace))
              (let (l (move control 1.0 (last trace)))
                   (append trace (assoc l :x (round (:x l))
                                          :y (round (:y l))
                                          :vx (round (:vx l))
                                          :vy (round (:vy l)))))))

(let ((tuple wait-trace guide)
      (let (L (read-lander)
            G (let (S (read-surface)) (future (make-guide L S)))
            loop (lambda t (let (g (deref G 100 nil))
                                (if g (tuple t g) (loop (approximate-move (Control 0 4) t))))))
           (loop (vector L)))))
