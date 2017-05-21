(let (tuple wait-trace guide) (loop (vector L))
     loop (lambda t (if-let g (deref G 100 nil) (tuple t g) (loop (approximate-move (Control 0 4) t)))))

(let (((tuple wait-trace guide) (loop ((t (vector L))) (if-let (g (deref G 100 nil)) (tuple t g) (loop (appriximate-move (Control 0 4) t))))))
  
  )

(let [[wait-trace guide] (loop [t [L]] (if-let [g (deref G 100 nil)] [t g] (recur (approximate-move (->Control 0 4) t))))])

(let (tuple wait-trace guide) (loop t (vector L) (let g (deref G 100 nil) (if g (tuple t g) (recur (approximate-move t (Control 0 4)))))))

(defn- approximate-move [^Control control trace]
  (let [l (move control 1.0 ^Lander (last trace))]
    (conj trace (assoc l :x (Math/round (:x l))
                         :y (Math/round (:y l))
                         :vx (Math/round (:vx l))
                         :vy (Math/round (:vy l))))))

(define approximate-move
  (lambda (type control Control) (type trace (List Lander))
    (let l (move control 1.0 (last trace))
      (append trace (assoc l :x (round (:x l))
                             :y (round (:y l))
                             :vx (round (:vx l))
                             :vy (round (:vy l)))))))
