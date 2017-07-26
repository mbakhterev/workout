(ns spoj.prime1)

(set! *warn-on-reflection* true)

(defn- unite [A B] (into A B))

(defn- sieve-bound [n] (+ 1 (long (Math/sqrt n))))

(defn- product-set-init [S p N] (if (S p) #{} (set (range (* p p) (+ 1 N) p))))

(defn- non-primes-init [N]
  (reduce
    (fn [S i] (unite S (product-set-init S i N)))
    (unite #{1} (set (range 4 (+ 1 N) 2)))
    (range 3 (sieve-bound N))))

(defn- set-sieve-init [N]
  (let [np (non-primes-init N)] (filter #(not (np %)) (range 1 N))))

(defmacro prime-gen [N] (vec (set-sieve-init (eval N))))

(def prime-vec (prime-gen (sieve-bound 1e9)))

(defn- offset [m p]
  (if (>= p m)
    (- (* 2 p) m)
    (let [r (mod m p)] (if (not= 0 r) (- p r) 0))))

(defn- non-primes-range [M N]
  (let [n-bits (+ 1 (- N M))]
    (reduce
      (fn [S p] (unite S (set (range (offset M p) n-bits p))))
      (if (== M 1) #{0} #{})
      (take-while (partial >= (sieve-bound N)) prime-vec))))

(defn- set-sieve-range [^long M ^long N]
  (let [np (non-primes-range M N)]
    (filter #(not (np (- % M))) (range M (+ 1 N)))))

(defn- process-pair []
  (let [[M N] (map read-string (clojure.string/split (read-line) #" "))]
    (print (clojure.string/join "\n" (set-sieve-range M N)) "\n\n")))

(defn- run [] (dorun (repeatedly (read-string (read-line)) process-pair)))

(run)
