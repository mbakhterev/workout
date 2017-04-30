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

(def ^:private ^:const test-id 0)
(def ^:private ^:const s-points (surface-points (:surface (test-data test-id))))
(def ^:private ^:const i-lander (load-lander (:lander (test-data test-id))))
(def ^:private ^:const l-pad (find-landing-pad s-points))
(def ^:private ^:const surface (surface-sections s-points))
(let [[l r] (surface-shell s-points l-pad)]
  (def ^:private ^:const shell (vec (concat l (list l-pad) r)))
  (def ^:private ^:const l-shell l)
  (def ^:private ^:const r-shell r))

(def ^:private ^:const stages (detect-stages i-lander l-shell l-pad r-shell)) 

; eval

(let [guide-control (search-guide stages i-lander)]
  (def ^:private ^:const guide (model-control guide-control stages i-lander)))

(def ^:private ^:const bad-cases
  [{:C (->Control 60 4)
    :S (nth stages 2) 
    :L #lander.Lander{:x 1500.0, :y 2514.4499999999994, :vx 100.0, :vy -37.10999999999999, :fuel 800, :control #lander.Control{:angle -15, :power 0}}}
   
   {:C (->Control 20 4)
    :S (nth stages 2)
    :L #lander.Lander{:x 1500.3393543299987, :y 2529.3060059296076, :vx 99.64872884586892, :vy -27.17071128706871, :fuel 790, :control #lander.Control{:angle 5, :power 4}}}])
 
(def ^:private ^:const bad (nth bad-cases 1))

(r/update-scene :surface surface)
(r/update-scene :landing-pad l-pad)
(r/update-scene :shell shell) 

(r/update-scene :traces (model-control (search-guide stages i-lander) stages i-lander))

(comment (count (keep (fn [[k v]] (if true [k (count (set v))]))
                      (group-by (comp :control :lander)
                                (:ok (group-by :state (keep (partial align-control (:L bad) (:S bad)) control-cloud))))))

         (count (hover-move-cloud i-lander (first stages)))

         (time (search-guide stages i-lander))
         (search-guide (drop 2 stages) (:L bad)))

(def ^:private ^:const d-lander (last (last guide)))
(def ^:private ^:const brk-stage (first (filter (comp (partial = :braking) :stage) stages)))

(let [d-lander (last (last guide))
      brk-stage (first (filter (comp (partial = :braking) :stage) stages))]
  (keep (partial brake-integrate brk-stage d-lander) control-cloud))

(map :stage stages)

(:lander (last (first (hover-cloud (first stages) i-lander))))

(hover-align-control (first stages) i-lander (->Control -90 4))

(hover-integrate-ok-one (first stages) i-lander)

(hover-integrate (first stages) i-lander (->HoverReduce [] nil) (->Control -90 4))

(time (map (comp :control :lander first) (filter (comp (partial = :out) :state first) (hover-cloud (first stages) i-lander))))
(count (hover-cloud (first stages) i-lander))
(count (map (comp :control :lander) (set (filter (comp (partial = :out) :state ) (map (partial hover-align-control (first stages) i-lander) control-cloud)))))

(let [stage (first stages)
      lander i-lander
      ctl (->Control -90 4)
      ma (hover-align-control stage )
      ])

(time (search-guide stages i-lander))

(let [lander #lander.Lander{:x 1000.0, :y 2653.6125, :vx 100.0, :vy -18.555, :fuel 800, :control #lander.Control{:angle -90, :power 0}}
      stage (second stages)
      ctl (->Control -15 3)]
  (reduce (partial hover-integrate stage lander) {:moves [] :outs #{}} control-cloud)
  )

(doseq [i (range 4)] (time (search-guide stages i-lander)))

(map (partial hover-integrate (first stages) i-lander) (hover-control-cloud (first stages) i-lander))

(count (hover-control-cloud (first stages) i-lander))
