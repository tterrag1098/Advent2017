(ns advent-2017.day06)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

(def input (vec (map #(Integer/parseInt %) (str/split "11 11 13 7 0 15 5 5 4 4 1 1 7 1 15 11" #"\s"))))

;; Increment i, modulo the size of coll
(defn inc-wrap [i coll] 
  (mod (inc i) (count coll)))

;; Distribute amt memory blocks into blocks, starting at position idx
(defn distr [blocks idx amt]
  (if (zero? amt)
    blocks
    (recur (assoc blocks idx (inc (nth blocks idx))) (inc-wrap idx blocks) (dec amt))))

(defn part1 [in]
  ((fn redis [blocks history acc] ; Redistribute memory blocks recursively
     (let [ idx (core/first-idx #(= (apply max blocks) %) blocks) ; Find the first instance of the max value
            val (nth blocks idx) ] ; Grab that max value
       (if (contains? history blocks) ; If this state is already in the history, we are finished
         { :steps acc :result blocks } ; So return a map of the current state
         ; Otherwise, set the current index to 0, call distr with index+1 (wrapped), add this state to the history, and recurse
         (recur (distr (assoc blocks idx 0) (inc-wrap idx blocks) val) (conj history blocks) (inc acc)))))
  in #{} 0))

;; Wonderful hack, just call part 1 twice, second time on the first time's output!
(defn part2 [in]
  (part1 ((part1 in) :result)))

(core/do-parts part1 part2 input)