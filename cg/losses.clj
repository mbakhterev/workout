(ns losses)

(defn- dump [& args] (binding [*out* *err*] (apply println args)))

(comment (defn supremum-at [predicate values]
           (transduce
             (map-indexed vector)
             (completing (fn [current n] (if (predicate (second n) (second current)) n current)))
             [0 (first values)]
             values))

         (defn -main [& args]
           (let [V '(3 2 10 7 15 14) ; (repeatedly (read) read)
                 [ith M] (supremum-at > V)
                 [jth m] (supremum-at < V)
                 after-max (drop (+ 1 ith) V)
                 before-min (take jth V)]
             (dump :max ith M)
             (dump :min jth m)
             (println (min (- m (if (empty? before-min) m (apply max before-min)))
                           (- (if (empty? after-max) M (apply min after-max)) M))))))


(defrecord State [^long best-max ^long best-min ^long current-max ^long current-min])

(defn- step ^State [^State {b-max :best-max b-min :best-min
                            c-max :current-max c-min :current-min
                            :as ST}
                    ^long v]
  (cond
    ; Если текущее значение больше текущего максимума, который больше лучшего
    ; максимума, то надо начинать новую развилку, потому что, если повезёт и
    ; найдётся большое падение, то оно будет больше от v, чем от текущего
    ; максимума. Текущая развилка начинается с current-min и current-max равных
    ; v
    (> v c-max) (assoc ST :current-max v :current-min v)

    ; Если текущее значение v меньше лучшего минимума b-min, то надо обновлять
    ; лучший максимум на текущий (потому что c-max >= b-max), а минимумы на v.
    (< v b-min) (assoc ST :best-max c-max :best-min v :current-min v)

    ; Если текущее значение v меньше текущего минимума c-min (и по предыдущему
    ; случаю v >= b-max), то надо обновить текущий минимум, и если новое падение
    ; на текущих максимуме и минимуме больше лучшего текущего, надо обновить
    ; лучшие показатели
    (< v c-min) (let [b-drop (- b-max b-min)
                      c-drop (- c-max v)]
                  (if (> c-drop b-drop)
                    (assoc ST :best-max c-max :best-min v :current-min v)
                    (assoc ST :current-min v)))
    :else ST))

(defn -main [& args]
           (let [V '(3 2 10 7 15 14) ; (repeatedly (read) read)
                 f (first V)
                 S (reduce step (->State f f f f) V)]
             (println (- (:best-min S) (:best-max S)))))
