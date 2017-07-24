(ns geometry)

(set! *warn-on-reflection* true)

; Структуры данных. Интуитивно могут быть полезны и повысить какую-нибудь
; эффективность

(defrecord Point [^double x
                  ^double y])

; Отрезок поверхности. k и mx - это наклон и середина отрезка по оси x.

(defrecord Section [^double ax ^double ay
                    ^double bx ^double by
                    ^double k
                    ^double mx])

(defrecord Landscape [^geometry.Section landing-pad
                      left-rock
                      right-rock
                      raw-surface])

(defn- make-section [^Point a ^Point b]
  (->Section (:x a) (:y a)
             (:x b) (:y b)
             (double (/ (- (:y b) (:y a))
                        (- (:x b) (:x a))))
             (+ (:x a)
                (/ (- (:x b) (:x a)) 2.0))))

(defn- surface-points [raw-numbers]
  (map (fn [p] (apply ->Point p)) (partition 2 raw-numbers)))

(defn- surface-sections [points]
  (map (fn [s] (apply make-section s)) (partition 2 1 points)))

(defn- find-landing-pad [points]
  (letfn [(is-pad ([[a b]] (< -0.01 (- (:y a) (:y b)) 0.01)))]
    (apply make-section (first (filter is-pad (partition 2 1 points))))))

(defn- surface-shell [points landing]
  (letfn [(monotonize [points]
            (loop [[p & P] (rest points)
                   max-y (:y (first points))
                   R [(first points)]]
              (cond
                ; Обработка последней точки. Если она выше чем предыдущий
                ; максимальный уровень, то включаем её в список. Если нет, то
                ; накрываем поверхность горизонтальным сегментом на
                ; максимальном уровне
                (empty? P) (conj R (if (> (:y p) max-y) p (->Point (:x p) max-y)))
                
                ; Обновление максимума с добавлением точки в результат
                (> (:y p) max-y) (recur P (:y p) (conj R p))

                ; Пропускаем точку
                :else (recur P max-y R))))

          (sectionize [points]
            (map (fn [s] (apply make-section s)) (partition 2 1 points)))]
    (let [l-points (filter (fn [p] (<= (:x p) (:ax landing))) points)
          r-points (filter (fn [p] (>= (:x p) (:bx landing))) points)
          l-shell (reverse (monotonize (reverse l-points)))
          r-shell (monotonize r-points)]
      [(vec (sectionize l-shell)) (vec (sectionize r-shell))]))) 

(defn detect-landscape [surface-data]
  (let [points (surface-points surface-data)
        pad (find-landing-pad points)
        [l-rock r-rock] (surface-shell points pad)]
    (->Landscape pad l-rock r-rock (surface-sections points))))
