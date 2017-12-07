(ns advent-2017.day04)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

(def input (map #(str/split %1 #"\s") (core/read-input "day4.txt")))

(defn part1
  [in]
  (count (filter #(apply distinct? %) input)))

(defn part2
  [in]
  (count (filter #(apply distinct? %) (map #(map sort %) input))))

(core/do-parts part1 part2 input)