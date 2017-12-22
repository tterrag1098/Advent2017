(ns advent-2017.day22)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

(def input (map (partial apply vector) (core/read-input "day22.txt")))
(def start [(/ (dec (count input)) 2) (/ (dec (count (first input))) 2)])
(def states
  (->> input
       (map-indexed (fn [x r] (map-indexed #(vector [%1 x] %2) r)))
       (apply concat)
       (into {})))

(defn get-state [states x y] (states [x y] \.))
(defn infected [states x y] (= \# (get-state states x y)))
(defn move-x [dir x] (if (= dir 1) (inc x) (if (= dir 3) (dec x) x)))
(defn move-y [dir y] (if (= dir 2) (inc y) (if (= dir 0) (dec y) y)))

(defn step 
  ([states x y dir count amt]
    (if (zero? amt) count
      (let [ i (infected states x y)
             dir (mod (if i (inc dir) (+ dir 3)) 4) ]
        (recur (assoc states [x y] (if i \. \#)) (move-x dir x) (move-y dir y) dir (if i count (inc count)) (dec amt))))))           

(defn part1 [in]
  (step in (first start) (second start) 0 0 10000))

(defn step-2
  ([states x y dir count amt]
    (if (zero? amt) count
      (let [ s (get-state states x y)
             dir (mod (case s
                        \F (bit-flip dir 1)
                        \# (inc dir)
                        \. (+ dir 3)
                        dir) 4) 
             ns (case s \. \W \W \# \# \F \F \.) ]
        (recur (assoc states [x y] ns) (move-x dir x) (move-y dir y) dir (if (= \# ns) (inc count) count) (dec amt))))))           

(defn part2 [in]
  (step-2 in (first start) (second start) 0 0 10000000))

(core/do-parts part1 part2 states)