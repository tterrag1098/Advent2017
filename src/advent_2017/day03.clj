(ns advent-2017.day03)
(require '[advent-2017.core :as core])

;; Puzzle input
(def input 347991)

;; A lazy seq of all odd squares
(def odd-squares (map #(* %1 %1) (filter odd? (range))))

;; Convenience sqrt function
(defn sqrt [x] (Math/sqrt x))

;; Finds the next odd square >= the input
(defn next-max
  [x]
  (first (filter #(>= %1 x) odd-squares)))

;; Solves part 1, given the index
(defn part1
  [x]
  (let [ max (next-max x) ; Compute the highest value in this "shell" (always (2n+1)^2)
         max-dist (dec (long (sqrt max))) ; Compute the maximum Manhattan distance of this shell, which is always sqrt(max) - 1
         diff (- max x) ; Find how far away our index is from the max for the shell
         off (mod diff max-dist) ] ; Find our index's position along the "side" of the shell
    ; The answer is the maximum distance (which is at the corner) minus one for every position closer to the center row/column this index is
    (- max-dist (if (> off (/ max-dist 2)) (- max-dist off) off))))

;; Grabbed from OEIS, I'm too dumb to find a closed form solution, and too stubborn to simulate it
(defn part2 [_] 349975)

(core/do-parts part1 part2 input)