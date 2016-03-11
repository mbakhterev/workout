(ns Player (:gen-class))

(defn- between [u v] (let [a (min u v) b (max u v)] (+ a (quot (- b a) 2))))

(defn- adjust [S bat-d bat-c b m]
  ; S: состояние
  ; bat-d: смещение от текущей координаты бэтмана
  ; bat-c: селектор координаты для бэтмана
  ; b: граница для поиска
  ; m:  обновляемая координата текущего окна сканирования
  (let [bat (bat-d (S bat-c)) border (S b)]
    (assoc S m bat bat-c (between bat border))))

(defn- up [S] (adjust S dec :bat-y :up-y :down-y))
(defn- down [S] (adjust S inc :bat-y :down-y :up-y))
(defn- left [S] (adjust S dec :bat-x :up-x :down-x))
(defn- right [S] (adjust S inc :bat-x :down-x :up-x))

(def ^:const M {\U up \D down \L left \R right})

(defn- decode [D] (reduce (fn [f d] (comp f (M d))) identity (str D)))

(defn- next-state [D S] (assoc ((decode D) S) :bomb-dir D))

(defn -main [& args]
  (let [[W H N X0 Y0] (repeatedly 5 read)]
    ; W: width of the building.
    ; H: height of the building.
    ; N: maximum number of turns before game over.
    (loop [S (next-state (read) {:up-x 0
                                 :up-y 0
                                 :down-x (dec W)
                                 :down-y (dec H)
                                 :bat-x X0
                                 :bat-y Y0
                                 :bomb-dir nil})]
      ; BD: the direction of the bombs from batman's current location (U, UR, R,
      ; DR, D, DL, L or UL)
        
      (binding [*out* *err*] (println W H N X0 Y0 "bomb dir:" (S :bomb-dir)))
        
      ; the location of the next window Batman should jump to.
      (println (S :bat-x) (S :bat-y))

      (recur (next-state (read) S)))))
