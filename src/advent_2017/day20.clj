(ns advent-2017.day20
  (:import (advent_2017 Day20Part2)))
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

(def input 
  (->> (core/read-input "day20.txt")
       (map (fn [s] 
              (-> s
                  (str/replace #"[pva<>=]" "")
                  (str/replace #"," " ")
                  (str/split #"\s+")
                  (->> (map #(Integer/parseInt %))
                       (partition 3)
                       (map vec)
                       (zipmap [:p :v :a])))))
       (vec)))

(defn abs [n] (if (< n 0) (- n) n))
(defn mag [v] (apply + (map abs v)))

(defn find-closest [particles]
  (apply min-key #(mag (% :p)) particles))

(defn index-of [particles p] ((clojure.set/map-invert (into {} (map-indexed #(vector %1 %2) particles))) p))

(defn simulate-one [in]
  (map (fn [p] 
         (-> p
             (assoc :v (map-indexed #(+ %2 (nth (p :a) %1)) (p :v)))
             (assoc :p (map-indexed #(+ %2 (nth (p :v) %1)) (p :p)))))
       in))

(defn simulate [in amt]
  (if (zero? amt) in
    (recur (simulate-one in) (dec amt))))

(defn find-mins [particles k]
  (as-> particles $ 
        (mag ((apply min-key #(mag (% k)) $) k))
        (filter #(= (mag (% k)) $) particles)))

(defn part1 [in]
  (as-> in $
        (simulate $ 1000)
        (index-of $ (find-closest $))))

(defn part2 [in] "Run the Day20Part2 Java program!")

(core/do-parts part1 part2 input)