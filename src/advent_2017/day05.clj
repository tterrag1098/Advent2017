(ns advent-2017.day05)
(require '[clojure.string :as str])

(def input (vec (map #(Integer/parseInt %) (str/split-lines (slurp "resources/day5.txt")))))

(defn jump [in f] (jump-r in 0 0 f))

(defn jump-r [in idx acc f]
 (if (>= idx (count in))
   acc
   (let [val (nth in idx)]
     (recur (assoc in idx (f val)) (+ idx val) (inc acc)))))

(defn part1 [in] (jump in inc))
(defn part2 [in] #(if (>= val 3) (dec val) (inc val)))

(do
  (println (str "Part 1: " (part1 input)))
  (println (str "Part 2: " (part2 input))))