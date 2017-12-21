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

(def rules
  (into {}
    (apply concat 
      (map (fn [rule]
             (let [m (first rule) ]
               (as-> m $
                     (vector $ (flip $ true))
                     (iterate (partial map rotate) $)
                     (take 4 $)
                     (apply concat $)
                     (map #(vector % (second rule)) $)))) input))))

(defn vec-split [idx v] (if (>= idx (count v)) v [(subvec v 0 idx) (subvec v idx)]))

(defn partition-mat [m row col] 
  (as-> m $
     (mapcat (partial apply map vector)
             (partition row (mapv (partial partition col) $)))
     (partition (/ (count m) col) $)))

(defn merge-horiz [mats]
  (vec (map-indexed 
         (fn [i m] (vec (apply concat m (map #(nth % i) (rest mats))))) 
         (first mats))))

(defn merge-corners [mats]
  (apply concat (map merge-horiz mats)))

(defn enhance
  ([rules grid times] (last (take (inc times) (iterate #(enhance rules %) grid))))
  ([rules grid]
    (if (>= (/ (count grid) 2) 2)
      (let [ split (if (zero? (mod (count grid) 2)) 2 3) ]
        (merge-corners (map #(map (partial enhance rules) %) (partition-mat grid split split))))
      (rules grid))))

(defn on-after [rules amt] (count (filter (partial = \#) (flatten (enhance rules grid amt)))))

(defn part1 [in] (on-after in 5))
(defn part2 [in] (on-after in 18))

(core/do-parts part1 part2 rules)