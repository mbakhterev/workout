(ns Solution (:gen-class))

; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.

(defn- parse-float ^double [^String s]
  (read-string (clojure.string/replace s #"," ".")))

(defn- distance [lon lat P]
  (let [x (* (- (:lon P) lon) (Math/cos (* 0.5 (+ (:lon P) lon))))
        y (- (:lat P) lat)]
    (Math/sqrt (+ (* x x) (* y y)))))

(defn -main [& args]
  (let [LON (parse-float (read-line))
        LAT (parse-float (read-line))
        DEF-RAW (repeatedly (read-string (read-line)) read-line)
        DEF (map (comp (fn [d] {:name (d 1) :lon (parse-float (d 4)) :lat (parse-float (d 5))})
                       #(clojure.string/split % #";"))
                 DEF-RAW)
        D (map (fn [d] (assoc d :distance (distance LON LAT d))) DEF)]

    (comment (binding [*out* *err*]
               (println LAT (type LAT) \newline
                        LON \newline
                        DEF-RAW \newline
                        DEF \newline
                        D)))

    ; Write answer to stdout
    (println (:name (first (sort-by :distance D))))))
