(ns advent-2017.day14)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])
(require '[advent-2017.day10 :as day10])

(def input "xlqgujun")

(def hash-inputs (map #(str input "-" %) (range 128)))

(def hashes (map #(day10/dense-hash
                    (day10/do-hash day10/data (day10/get-lengths %) 64))
                 hash-inputs))

(def grid 
  (map (fn [n] (apply str (map #(str/replace (format "%8s" (Integer/toBinaryString %)) \  \0) n))) hashes))

(defn part1 [in] (reduce + (map (fn [s] (count (filter #(= \1 %) s))) in)))

(defn add-if-present
  [seen grid x y]
  (if (or (< x 0) (< y 0) (> x 127) (> y 127)) seen
    (let [ val (nth (nth grid y) x) ]
      (if (= val \1) (conj seen [x y]) seen))))

(defn explore-group
  ([grid seen x y] (explore-group grid seen (list [x y])))
  ([grid seen search]
    (if (empty? search) seen
      (let [ [x y] (peek search)
             found (-> #{}
						            (add-if-present grid (inc x) y)
						            (add-if-present grid (dec x) y)
						            (add-if-present grid x (inc y))
						            (add-if-present grid x (dec y))) 
             new (filter #(not (contains? seen %)) found) ]
        (recur grid (clojure.set/union seen found) (if (empty? new) (pop search) (apply conj (pop search) new)))))))

(defn part2 [in]
  ((fn [in seen x y groups]
     (do (prn x y groups) (Thread/sleep 100)
     (let [ found (explore-group in seen x y) ]
       (if (> y 127) groups
         (recur in found (mod (inc x) 128) (if (= x 127) (inc y) y) (if (not= (count seen) (count found)) (inc groups) groups))))))
    in #{} 126 126 0))

;(core/do-parts part1 part2 grid)