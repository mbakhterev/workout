(ns Solution (:gen-class))

(defn- offset [T l]
  (let [n (dec (count T))
        i (- (int (first (clojure.string/capitalize l)))
             (int \A))]
    (if (<= 0 i (dec n)) i n)))

(defn- glyph [T l] (get-in T [(offset T l)]))

(defn- pad [T]
  (letfn [(pad-to [l s]
            (let [d (- l (count s))]
              (if (< 0 d) (apply str s (repeat d \space)) s)))]
    (map (partial pad-to (apply max (map count T))) T)))

(defn -main[& args]
  (let [[L H] (repeatedly 2 (comp read-string read-line))
        W (read-line)
        M (pad (repeatedly H read-line))
        T (vec (partition-all L (apply map vector M)))
        R (mapcat (partial glyph T) W)
        A (apply map vector R)]

    (comment (binding [*out* *err*]
               (dorun (map prn M))
               (dorun (map println A))))

    (dorun (map (comp println (partial apply str)) A))))
