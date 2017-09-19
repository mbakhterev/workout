(ns main (:gen-class)
         (:require [lander :refer :all]
                   [geometry :refer :all]
                   [render :refer :all]))

(set! *warn-on-reflection* true)

(defn- dump [& args] (binding [*out* *err*] (apply println args)))
(defn- read-surface [] (let [N (read)] (doall (repeatedly (* 2 N) read))))
(defn- read-lander [] (doall (repeatedly 7 read)))

(defn- sketch-landscape [^geometry.Landscape scape]
  (update-scene :surface (:raw-surface scape))
  (update-scene :landing-pad (:landing-pad scape)) 
  (update-scene :shell (concat (:left-rock scape)
                               [(:landing-pad scape)]
                               (:right-rock scape))))

; Структура этого всего хозяйства:
;   guides - (список (список (список Lander)))
;   traces - (список (список Lander))

(let [state (atom {:guides [] :traces [] :stages []})]
  (defn- reset-state []
    (reset! state {:guides [] :traces [] :stages []})
    (update-scene :guides nil)
    (update-scene :traces nil)
    (update-scene :stages nil)) 

  (defn- next-guide [g]
    (swap! state (fn [st] (assoc st :guides (conj (:guides st) g))))) 

  (defn- next-trace [^lander.Lander l]
    (swap! state (fn [st] (assoc st :traces (conj (:traces st) [l])))))

  (defn- next-stages [s]
    (swap! state (fn [st] (assoc st :stages (conj (:stages st) s)))))

  (defn- unpack-traces [t]
    (if (and (not (empty? t))
             (not (empty? (last t)))
             (= lander.Lander (type (last (last t)))))
      ; [(drop-last t) (last t)]
      [(vec (drop-last t)) (vec (last t))]))

  (defn- check-guides [g]
    (and (not (empty? g))
         (not (empty? (last g)))
         (not (empty? (last (last g))))
         (= lander.Lander (type (last (last (last g)))))))

  (defn- sketch-state []
    (let [st (deref state)]
      (if (check-guides (:guides st)) (update-scene :guides (:guides st)))
      (if (unpack-traces (:traces st)) (update-scene :traces (:traces st)))
      (if (not (empty? (:stages st))) (update-scene :stages (first (:stages st))))))

  (defn- trace-move [^lander.Control control]
    (swap! state (fn [st] (let [[prev curr :as traces] (unpack-traces (:traces st))]
                            (assert traces)
                            (assoc st :traces
                                   (conj prev (conj curr (move control 1.0 ^Lander (last curr)))))))))
  
  (defn- approximate-last []
    (let [[prev curr :as traces] (unpack-traces (:traces (deref state)))]
      (assert traces)
      (let [l ^lander.Lander (last curr)]
        (assoc l :x (Math/round ^double (:x l))
                 :y (Math/round ^double (:y l))
                 :vx (Math/round ^double (:vx l))
                 :vy (Math/round ^double (:vy l)))))))

(defn- make-guide [^lander.Lander {x :x vx :vx :as l-init}
                   ^geometry.Landscape scape]
  (let [stages (make-stages x vx scape)]
    (next-stages stages)
    (sketch-state)
    (debugln :make-guide stages)
    (let [guide (search-guide stages l-init)]
      (next-guide guide)
      (sketch-state)
      (flatten-guide guide))))

(def ^:private ^:const quanta 128)

(defn- wait-loop [^geometry.Landscape scape
                  ^lander.Control control
                  ^lander.Lander lander]
  (next-trace lander)
  (loop [l lander G (future (make-guide lander scape)) steps 0]
    (if-let [g (deref G quanta nil)] 
      (do (dump "guide computation is DONE." "steps:" steps "guide length:" (count g))
          (if (empty? g)
            (do (dump "guide computation is FAILED. Continuing with routine")
                (trace-move control)
                (let [tl (approximate-last)]
                  (recur tl (future (make-guide tl scape)) 0)))
            [l g]))
      (do (dump "waiting for guide with control:" control)
          (trace-move control)
          (recur (approximate-last) G (+ 1 steps))))))

(def ^:private ^:const tolerable-drift (* 4.0 4.0))

(defn- guide-loop [^lander.Lander lander guide]
  (next-trace lander)
  (loop [l lander steps 0]
    (if-let [[delta control] (along-guide l guide)]
      (if (> delta tolerable-drift)
        (do (dump "guide drift is too large. Correction is needed."
                  "delta:" delta
                  "steps:" steps
                  "x:" (:x (approximate-last))
                  "lander:" l)
            l)
        (do (dump "delta is ok:" delta)
            (trace-move control)
            (recur (approximate-last) (+ 1 steps))))
      (do (dump "guide following is DONE")
          (sketch-state)
          nil))))

; Тестовые данные
(def ^:private ^:const test-data [{:surface [0 1000 300 1500 350 1400 500 2000
                                             800 1800 1000 2500 1200 2100 1500 2400
                                             2000 1000 2200 500 2500 100 2900 800
                                             3000 500 3200 1000 3500 2000 3800 800
                                             4000 200 5000 200 5500 1500 6999 2800]
                                   :lander [500 2700 100 0 800 -90 0]}
                                  
                                  {:surface [0 1000 300 1500 350 1400 500 2100
                                             1500 2100 2000 200 2500 500 2900 300 3000
                                             200 3200 1000 3500 500 3800 800 4000 200
                                             4200 800 4800 600 5000 1200 5500 900
                                             6000 500 6500 300 6999 500]
                                   :lander [6500 2700 -50 0 1000 90 0]}
                                  
                                  {:surface [0 100 1000 500 1500 1500 3000 1000 4000 150 5500 150 6999 800]
                                   :lander [6500 2800 -90 0 750 90 0]}])

(comment (let [T (test-data 1)
               S (make-landscape (:surface T))
               L (make-lander (:lander T))
               stages (make-stages (:x L) (:vx L) S)
               guide (search-guide stages L)]
           (reset-state)
           (sketch-landscape S)
           guide
           stages
           ))

(defn -main [& args]
  (reset-state)
  (let [T (test-data 1)
        S (make-landscape (:surface T))
        L (make-lander (:lander T))]
    (sketch-landscape S)
    (loop [l L]
      (let [[lw g] (wait-loop S (->Control 0 4) l)]
        (dump "guide length:" (count g))
        (if-let [lg (guide-loop lw g)]
          (recur lg)
          (approximate-last)))))) 
