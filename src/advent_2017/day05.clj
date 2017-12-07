(ns advent-2017.day05)
(require '[advent-2017.core :as core])

(def input (vec (map #(Integer/parseInt %) (core/read-input "day5.txt"))))

(defn jump-r [in idx acc f]
 (if (>= idx (count in))
   acc
   (let [val (nth in idx)]
     (recur (assoc in idx (f val)) (+ idx val) (inc acc) f))))

(defn jump [in f] (jump-r in 0 0 f))

(defn part1 [in] (jump in inc))
(defn part2 [in] (jump in #(if (>= % 3) (dec %) (inc %))))

(core/do-parts part1 part2 input)