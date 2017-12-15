(ns advent-2017.core)
(require '[clojure.string :as str])

;; Perform both parts with the given input
(defn do-parts 
  ([p1 p2 input] { :part1 (p1 input) :part2 (p2 input) })
  ([p1 p2 in1 in2] { :part1 (p1 in1) :part2 (p2 in2) }))

;; Read from a file and return the lines as a string seq
(defn read-input [f]
  (str/split-lines (slurp (str "resources/" f))))

;; Find the index of the first element in coll matching pred
(defn first-idx [pred coll]
   (first (keep-indexed #(when (pred %2) %1) coll)))

;; Perform f on values of a map
(defn update-values [m f & args]
 (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))