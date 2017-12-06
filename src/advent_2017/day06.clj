(ns advent-2017.day06)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

(def input (vec (map #(Integer/parseInt %) (str/split "11 11 13 7 0 15 5 5 4 4 1 1 7 1 15 11" #"\s"))))

(defn first-idx [pred coll]
  (first (keep-indexed #(when (pred %2) %1) coll)))

(defn inc-wrap [i coll] 
  (mod (inc i) (count coll)))

(defn distr [blocks idx amt]
  (if (zero? amt)
    blocks
    (do
      (recur (assoc blocks idx (inc (nth blocks idx))) (inc-wrap idx blocks) (dec amt)))))

(defn part1 [in]
  ((fn redis [blocks history acc]
     (let [ idx (first-idx #(= (apply max blocks) %) blocks)
            val (nth blocks idx) ]
       (if (contains? history blocks)
         { :steps acc :result blocks }
         (recur (distr (assoc blocks idx 0) (inc-wrap idx blocks) val) (conj history blocks) (inc acc)))))
  in #{} 0))

(defn part2 [in]
  (part1 ((part1 in) :result)))

(core/do-parts part1 part2 input)