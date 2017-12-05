(ns advent-2017.day04)
(require '[clojure.string :as str])

(def input (map #(str/split %1 #"\s") (str/split-lines (slurp "resources/day4.txt"))))

(defn part1
  [in]
  (count (filter #(apply distinct? %) input)))

(defn part2
  [in]
  (count (filter #(apply distinct? %) (map #(map sort %) input))))

(defn run []
  (do
	  (println (str "Part 1: " (part1 input)))
	  (println (str "Part 2: " (part2 input)))))

(run)