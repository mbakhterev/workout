(ns records)
; Структуры данных. Интуитивно могут быть полезны и повысить какую-нибудь
; эффективность

(defrecord Point [^double x
                  ^double y])

; Отрезок поверхности. k и mx - это наклон и середина отрезка по оси x.

(defrecord Section [^double ax ^double ay
                    ^double bx ^double by
                    ^double k
                    ^double mx])

(defrecord Lander [^double x
                   ^double y
                   ^double vx
                   ^double vy
                   ^long fuel
                   ^long angle
                   ^long power
                   ^boolean alive])

(defn make-section [^Point a ^Point b]
  (Section. (:x a) (:y a)
            (:x b) (:y b)
            (double (/ (- (:y b) (:y a))
                       (- (:x b) (:x a))))
            (+ (:x a)
               (/ (- (:x b) (:x a)) 2.0))))  

; Размер ячейки сетки в условных единицах. Ячейка - это квадрат с ребром длиной
; dG. Почему dG = 20? Чтобы половина ребра, rG = 10. Почему rG = 10? От балды
(def ^:const dG 20.0)
(def ^:const rG (/ dG 2.0)) 

(defrecord Grid [^double baseline rows])
(defrecord Row [^double left cells])
(defrecord Cell [vx vy])

(defn make-cell [vx-range vy-range] (Cell. (long-array (/ vx-range 32)) 
                                           (long-array (/ vy-range 32))))
