(ns spoj.prime1)

(set! *unchecked-math* true)

; (set! *flush-on-newline* false)

(defn- sieve-bound [N] (+ 1 (long (Math/sqrt N))))

(defn- mod-test [P n] (not (some #(and (not= n %) (zero? (mod n %))) P)))

(defn- prime-reduce [P n] (if (mod-test P n) (conj P n) P))

(defn- prime-sieve [N] (reduce prime-reduce [2] (range 3 N 2)))

(defmacro prime-gen [N] (vec (prime-sieve (eval N))))

(def prime-vec (prime-gen (sieve-bound 1000000000)))

(defn- sieve-range [M N]
  (let [P (take (+ 1 (Math/sqrt N)) prime-vec)
        R (range (max M 2) (inc N))]
    (filter (partial mod-test P) R)))

(defn- to-int [s] (Integer/parseInt s))

(defn- process-pair []
  (let [[M N] (map to-int (.split (read-line) " "))]
    (print (clojure.string/join \newline (sieve-range M N)) "\n\n")))

(defn- run [] (dorun (repeatedly (to-int (read-line)) process-pair)))

(run)
