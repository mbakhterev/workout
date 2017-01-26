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
                   ^double dx
                   ^double dy
                   ^long fuel
                   ^long angle
                   ^long power])  

(defn make-section [^Point a ^Point b]
  (Section. (:x a) (:y a)
            (:x b) (:y b)
            (double (/ (- (:y b) (:y a))
                       (- (:x b) (:x a))))
            (+ (:x a)
               (/ (- (:x b) (:x a)) 2.0))))  
