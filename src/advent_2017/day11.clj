(ns advent-2017.day11)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

(def input (map str/trim (str/split (slurp "resources/day11.txt") #",")))

(def start { :x 0 :y 0 :z 0})

;; Hexagonal grid mapped to 3D coordinates http://keekerdc.com/2011/03/hexagon-grids-coordinate-systems-and-distance-calculations/
(defn move
  [ins coords]
  (let [ x (coords :x) 
         y (coords :y) 
         z (coords :z) ]
    (case ins
      "n"  (assoc coords :x (dec x) :y (inc y))
      "s"  (assoc coords :x (inc x) :y (dec y))
      "ne" (assoc coords :y (inc y) :z (dec z))
      "se" (assoc coords :x (inc x) :z (dec z))
      "nw" (assoc coords :x (dec x) :z (inc z))
      "sw" (assoc coords :y (dec y) :z (inc z)))))

;; Hexagonal distance forumla
(defn dist
  [coords]
  (apply max (map #(Math/abs %) (vals coords))))

;; Walk the hexagonal grid with the given instructions
(defn walk
  ([ins] (walk ins start start))
  ([ins coords maxes]
    (if (empty? ins) { :result coords :max maxes }
      (recur (rest ins) (move (first ins) coords) (reduce-kv #(assoc %1 %2 (max %3 (coords %2))) {} maxes)))))

(defn part1 [in]
  (dist ((walk in) :result)))

(defn part2 [in]
  (dist ((walk in) :max)))

(core/do-parts part1 part2 input)