(ns main (:gen-class)
         (:require [lander :refer :all]
                   [geometry :refer :all]
                   [render :as r]))

(defn- dump [& args] (binding [*out* *err*] (apply println args)))
(defn- read-surface [] (let [N (read)] (doall (repeatedly (* 2 N) read))))
(defn- read-lander [] (doall (repeatedly 7 read)))

(defn -main [& args]
  (let [P (read-surface)
        L (read-lander)]
    (dump "surface: " P)
    (dump "lander: " L)))

; Тестовые данные
(def ^:private ^:const test-data [{:surface [0 1000 300 1500 350 1400 500 2000
                                             800 1800 1000 2500 1200 2100 1500 2400
                                             2000 1000 2200 500 2500 100 2900 800
                                             3000 500 3200 1000 3500 2000 3800 800
                                             4000 200 5000 200 5500 1500 6999 2800]
                                   :lander [500 2700 100 0 800 -90 0]}])


(defn- make-lander [L] (apply ->Lander (conj (vec (take 5 L)) (apply ->Control (drop 5 L)))))

(def ^:private ^:const test-id 0)
(def ^:private ^:const s-points (surface-points (:surface (test-data test-id))))
(def ^:private ^:const i-lander (make-lander (:lander (test-data test-id))))
(def ^:private ^:const l-pad (find-landing-pad s-points))
(def ^:private ^:const surface (surface-sections s-points))
(let [[l r] (surface-shell s-points l-pad)]
  (def ^:private ^:const shell (vec (concat l (list l-pad) r)))
  (def ^:private ^:const l-shell l)
  (def ^:private ^:const r-shell r))
 
(r/update-scene :surface surface)
(r/update-scene :landing-pad l-pad)
(r/update-scene :shell shell) 

(def ^:private ^:const stages (detect-stages i-lander l-shell l-pad r-shell))

(defn- trace-control [l angle power]
  (take-while (partial alive? shell) (reductions (wrap move 1.0) l (repeat [angle power]))))

(def ^:private ^:const hovers
  (reduce (integrate-wrap integrate-hover (first stages) i-lander)
          {}
          (for [a (range -90 91 5) p (range 5)] [a p])))

(def ^:private ^:const hovers-2
  (reduce (integrate-wrap integrate-hover (second stages) (first (get-landers false hovers)))
          {}
          (for [a (range -90 91 5) p (range 5)] [a p])))

(identity hovers-2)

(r/update-scene :traces (concat (comment (for [p (range 4) a (range -90 91 5)] (trace-control i-lander a p)))
                                (for [[{{a :angle p :power} :control} t] (list (first (get-landers true hovers)))]
                                  (take (+ 1 t) (trace-control i-lander a p)))
                                (for [[{{a :angle p :power} :control} t] (get-landers true hovers-2)]
                                  (take (+ 1 t) (trace-control n-lander a p)))))

(constraint (move -90 0 4.0 i-lander) l-pad)
(map second (get-landers true hovers-2))
(identity hovers-2)

(trace-control i-lander 90 20)

(map count (for [[{{a :angle p :power} :control} t] (get-landers true hovers)] (take t (trace-control i-lander a p))))

(time (count (for [p (range 4) a (range -90 91 5)] {:trace (trace-control i-lander a p)})))

(get-landers true (integrate-hover {} (first stages) i-lander (->Control 90 0)))
(get-landers false hovers)
(identity hovers)
(keep second (vals hovers))

(time (count (for [[{{a :angle p :power} :control} t] (get-landers true hovers-2)]
                                  (take (+ 1 t) (trace-control n-lander a p)))))

(def ^:private ^:const n-lander (first (get-landers false hovers)))

(doseq [p (range 4) a (range -90 91 5)]
  (println a p)
  (reduce (integrate-wrap integrate-hover (second stages) (first (get-landers false hovers))) {} (list [a p])))

(reduce (integrate-wrap integrate-hover (second stages) h-lander) {} (list [-90 0]))

(integrate-hover {} (second stages) n-lander (->Control -65 0))

(control-tune (:control n-lander) (->Control -65 0))

(approach-loop n-lander (second stages) (->Control -65 0))
(move (->Control -65 0) 1.0 n-lander)
(x-acceleration -65 0)
(y-acceleration -65 0)

(def ^:private ^:const t-lander
  #lander.Lander{:x 1000.0000000000005, :y 2672.0009667677978,
                 :vx 108.45640052594703, :vy -8.201858107192168,
                 :fuel 786, :control #lander.Control{:angle -30, :power 4}})

(constraint t-lander l-pad)

(braking-constraint-h-dx t-lander l-pad)
(descending-constraint-h (assoc t-lander :vy -48))
(identity l-pad)

(identity i-lander)

(count (reduce (integrate-wrap integrate-hover (first stages) i-lander) {}
        (for [a (range -90 91 5) p (range 5)] [a p])))

(count (for [a (range -90 91 5) p (range 5)] [a p]))

(map (juxt :stage :left?) (detect-stages i-lander l-shell l-pad r-shell))
