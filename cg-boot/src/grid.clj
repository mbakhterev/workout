(ns grid (:require [lander :refer :all]))

(set! *warn-on-reflection* true)

(defrecord Grid [^double  dG
                 ^double  dV
                 ^long    nV
                 ^double  baseline
                          rows])

(defrecord Row [^double left
                        cells])

(defrecord Cell [vx+ vx- vy+ vy-]) 

(def ^:private ^:const dG 20.0)
(def ^:private ^:const dV 1.0)
(def ^:private ^:const nV 50) 
(def ^:private ^:const fV 38.0)  

(defn- grid-ceil [dG x] (* dG (Math/ceil (/ x dG))))
(defn- grid-floor [dG x] (* dG (Math/floor (/ x dG))))

(defn- build-row [dG nV l-side r-side x-max height]
  (let [between (fn [a b h] (and (<= a h) (< h b)))
        xbyy (fn [y s] (+ (:ax s) (/ (- y (:ay s)) (:k s))))
        l (first (filter (fn [s] (between (:by s) (:ay s) height)) l-side))
        r (first (filter (fn [s] (between (:ay s) (:by s) height)) r-side))
        lx (grid-ceil dG (+ (if l (xbyy height l) 0.0) (* 2 dG)))
        rx (grid-floor dG (- (if r (xbyy height r) x-max) (* 2 dG)))
        n-cells (+ 1 (long (/ (- rx lx) dG))) ]
    (->Row lx (vec (repeatedly n-cells (fn [] (boolean-array (* 4 nV nV) true))))))) 

(defn- build-grid [dG dV nV l-rock r-rock l-pad lander x-max y-max]
  ;         dG, dV: размеры ячеек по пространству и по скоростям
  ;             nV: количество шагов по скорости в каждом направлении: up, down,
  ;                 left, right
  ; l-rock, r-rock: левая и правая стороны скалы
  ;          l-pad: посадочная площадка
  ;         lander: начальная позиция модуля  
  ;   x-max, y-max: размер пространства
  (let [l (move-back (->Lander (:mx l-pad) (:ay l-pad) 0.0 -35.0 0 0 4 true) 0 4)
        h (- (:y l) (* 0.25 dG))
        top (min (+ (:y lander) (* 4 dG)) y-max)]
    (->Grid dG dV nV h (mapv (partial build-row dG nV l-rock r-rock x-max) (range h top dG)))))

(defn- arrived? [dG dV T l]
  (and (<= (Math/abs ^double (- (:x T) (:x l))) dG)
       (<= (Math/abs ^double (- (:y T) (:y l))) dG)
       (<= (Math/abs ^double (- (:vx T) (:vx l))) dV)
       (<= (Math/abs ^double (- (:vy T) (:vy l))) dV)))

(defn- speed-free!? [nV dV ^booleans array l]
  (let [lvx (:vx l)
        lvy (:vy l)
        offset (* nV nV (+ (if (neg? lvx) 2 0)
                           (if (neg? lvy) 1 0)))
        nx (Math/ceil (/ (Math/abs ^double lvx) dV))
        ny (Math/ceil (/ (Math/abs ^double lvy) dV))
        idx (+ offset (* ny nV) nx)]
    (and (< nx nV)
         (< ny nV)
         (aget array idx)
         (not (aset array idx false)))))

(defn- grid-free!? [G l]
  (let [dg (:dG G)
        bl (:baseline G)
        R (:rows G)
        h (:y l)
        nr (Math/ceil (/ (- h bl) dg))]
    (and (>= h bl)
         (< nr (count R))
         (let [r (nth R nr)
               C (:cells r)
               lx (:left r)
               x (:x l) 
               nc (Math/ceil (/ (- x lx) dg))]
           (and (>= x lx)
                (< nc (count C))
                (speed-free!? (:nV G) (:dV G) (nth C nc) l))))))

(defn- landers-up [dG fV l-pad]
  (map (fn [x] (move-back (->Lander x (:ay l-pad) 0.0 (- fV) 0 0 0 true) 0 4))
       (range (+ (:ax l-pad) dG) (:bx l-pad) dG)))

(defn- paths-up [dG fV l-pad] (map list (landers-up dG fV l-pad)))

(defn- estimate [T l]
  (let [dx ^double (- (:x T) (:x l))
        dy ^double (- (:y T) (:y l))
        dvx ^double (- (:vx T) (:vx l))
        dvy ^double (- (:vy T) (:vy l))]
    (+ (Math/sqrt (+ (* dy dy)))
       (* 0.001 (+ (Math/abs dvx) (Math/abs dvy) (Math/abs dx) (Math/abs dy) (:fuel l))))))

(defn- path-estimate [T p] (estimate T (first p))) 

(comment (defn- new-grid [] (build-grid dG dV nV l-shell r-shell l-pad i-lander)))

(defn- mk-compare-path [T] (fn [x y] (if (< (path-estimate T x) (path-estimate T y)) -1 1)))

(defn- mk-pass-grid [G] (fn [p] (if (grid-free!? G (first p)) p)))

(defn- search [grid target paths N]
  (let [cmp (mk-compare-path target)
        pass (mk-pass-grid grid)]
    (loop [queue (sort-by (partial path-estimate target) paths)
           n N]
      (comment (println (map (partial path-estimate target) (take 4 queue))))
      (if-let [p (first queue)]
        (cond (zero? n)
              queue

              (arrived? (:dG grid) (:dV grid) target (first p))
              (list p)

              (grid-free!? grid (first p))
              (recur (sort-by (partial path-estimate target) (concat (rest queue) (path-cloud p))) (- n 1))

              :else
              (recur (rest queue) (- n 1))))))) 
