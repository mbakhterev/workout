(ns spoj.prime1)

(set! *warn-on-reflection* true)

(defn- sieve-bound [N] (+ 1 (long (Math/sqrt N))))

(defn- bit-sieve-init [N]
  (let [n-bound   (+ 1 N)
        prime-set (new java.util.BitSet n-bound)]
    (.flip prime-set 2 n-bound)
    (doseq [i (range 4 n-bound 2)] (.clear prime-set i))
    (doseq [p (range 3 (sieve-bound N))]
      (if (.get prime-set p)
        (doseq [q (range (* p p) n-bound (* 2 p))] (.clear prime-set q))))
    (filter #(.get prime-set %) (range 1 n-bound))))

(defmacro prime-gen [N] (vec (bit-sieve-init (eval N))))

(def prime-vec (prime-gen (sieve-bound 2147483647)))

(defn- offset [m p]
  (if (>= p m)
    (- (* 2 p) m)
    (let [r (mod m p)] (if (not= r 0) (- p r) 0))))

(defn- bit-sieve-range [^long M ^long N]
  (let [n-bits    (+ 1 (- N M))
        prime-set (boolean-array n-bits)]
    (aset-boolean prime-set 0 (== M 1))
    (doseq [p (take-while (partial >= (sieve-bound N)) prime-vec)
            q (range (offset M p) n-bits p)]
      (aset-boolean prime-set q true))
    (filter #(not (aget prime-set (- % M))) (range M (+ 1 N)))))

(defn- to-int [s] (Integer/parseInt s))

(defn- process-pair []
  (let [[M N] (map to-int (clojure.string/split (read-line) #" "))]
    (print (clojure.string/join \newline (bit-sieve-range M N)) "\n\n")))

(defn- run [] (dorun (repeatedly (to-int (read-line)) process-pair)))

(run)
