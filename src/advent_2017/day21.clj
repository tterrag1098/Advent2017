(ns advent-2017.day21)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

(def input 
  (->> (core/read-input "day21.txt")
        (map (fn [l] 
               (-> l
                   (str/split #"\s=>\s")
                   (->> (map (fn [r] 
                               (-> r 
                                   (str/split #"/")
                                   (->> (map #(vec (char-array %))))
                                   (vec))))) (vec)))) (vec)))

(def grid
[ [ \. \# \. ]
  [ \. \. \# ]
  [ \# \# \# ] ])

(defn rotate [mat]
  (apply mapv #(into [] %&) (reverse mat)))

(defn flip [mat updn]
  (if updn (vec (reverse mat)) (vec (map #(vec (reverse %)) mat))))

(defn find-rule [rules grid]
  (first (filter (fn [rule] 
                   (let [ m (first rule) ]
                     (as-> m $
                           (vector $ (flip $ true) (flip $ false))
                           (iterate #(map rotate %) $)
                           (take 4 $)
                           (apply concat $)
                           (conj $ (flip m true))
                           (conj $ (flip m false))
                           (some #{grid} $)))) rules)))

(defn vec-split [idx v] [(subvec v 0 idx) (subvec v idx)])

(defn partition-mat [m row col] 
  (mapcat (partial apply map vector) 
          (vec-split row (mapv (partial vec-split col) m))))

(defn merge-halves [m1 m2]
  (vec (map-indexed #(vec (concat %2 (nth m2 %1))) m1)))

(defn merge-corners [m1 m2 m3 m4]
  (concat (merge-halves m1 m2) (merge-halves m3 m4)))

(defn enhance
  ([rules grid times] (last (take (inc times) (iterate #(enhance rules %) grid))))
  ([rules grid]
    (let [ half (/ (count grid) 2) ]
      (if (>= half 2)
        (apply merge-corners (map #(enhance rules %) (partition-mat grid half half)))
        (->> grid
             (find-rule rules)
             second)))))

(defn part1 [in] (count (filter #(= \# %) (flatten (enhance in grid 5)))))

(defn part2 [in]
  )

(core/do-parts part1 part2 input)