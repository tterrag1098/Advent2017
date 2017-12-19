(ns advent-2017.day14)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])
(require '[advent-2017.day10 :as day10])

(def input "xlqgujun")

(def hash-inputs (map #(str input "-" %) (range 128)))

(def hashes (map #(day10/dense-hash (day10/do-hash day10/data (day10/get-lengths %) 64)) hash-inputs))

(def grid 
  (vec (map (fn [n] (vec (apply str (map #(str/replace (format "%8s" (Integer/toBinaryString %)) \  \0) n)))) hashes)))

(defn part1 [in] (reduce + (map (fn [s] (count (filter #(= \1 %) s))) in)))

(defn grid-get [grid x y]
  (if (or (< x 0) (< y 0) (> x 127) (> y 127)) \0
    (nth (nth grid y) x)))

(defn add-if-present
  [seen grid x y]
  (let [ val (grid-get grid x y) ]
    (if (= val \1) (conj seen [x y]) seen)))

(defn explore-group
  ([grid seen x y id] 
    (if (contains? seen [x y]) [grid seen] (explore-group grid seen (list [x y]) id)))
  ([grid seen search id]
    (if (empty? search) [grid seen]
      (let [ [x y] (peek search) ]
        (if (= \0 (grid-get grid x y)) (recur grid seen (pop search) id)
          (let [ found (-> #{}
						             (add-if-present grid (inc x) y)
						             (add-if-present grid (dec x) y)
						             (add-if-present grid x (inc y))
						             (add-if-present grid x (dec y))) 
                 new (filter #(not (contains? seen %)) found) ]
            (recur (assoc grid y (assoc (nth grid y) x id)) (conj (clojure.set/union seen found) (peek search)) (if (empty? new) (pop search) (apply conj (pop search) new)) id)))))))

(defn part2 [in]
  ((fn [in seen x y groups]
    (let [ res (explore-group in seen x y groups) ]
      (if (> y 127) groups
        (recur (first res) (second res) (mod (inc x) 128) (if (= x 127) (inc y) y) (if (not= (count seen) (count (second res))) (inc groups) groups)))))
    in #{} 0 0 0))

(core/do-parts part1 part2 grid)