(ns advent-2017.core)
(require '[clojure.string :as str])

;; Returns a map to nicely represent the two parts
(defn print-parts [res1 res2] { :part1 res1 :part2 res2 })

;; Perform both parts with the given input
(defn do-parts 
  ([p1 p2 input] (do-parts p1 p2 input input))
  ([p1 p2 in1 in2] (print-parts (p1 in1) (p2 in2))))

;; Read from a file and return the lines as a string seq
(defn read-input [f]
  (str/split-lines (slurp (str "resources/" f))))

;; Find the index of the first element in coll matching pred
(defn first-idx [pred coll]
   (first (keep-indexed #(when (pred %2) %1) coll)))

;; Perform f on values of a map
(defn update-values [m f & args]
 (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))