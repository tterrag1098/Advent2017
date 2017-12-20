(ns advent-2017.day20)
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

(defn find-closest [particles]
  (apply min-key (fn [p] (apply + (map #(Math/abs %) (p :p)))) particles))

;; Infinite loop because lazy, once it prints out the same value for a long time it can be assumed that this is the answer
(defn part1 [in]
  (do (prn ((clojure.set/map-invert (into {} (map-indexed #(vector %1 %2) in))) (find-closest in)))
      (recur (map (fn [p] 
                    (-> p
                         (assoc :v (map-indexed #(+ %2 (nth (p :a) %1)) (p :v)))
                         (assoc :p (map-indexed #(+ %2 (nth (p :v) %1)) (p :p)))))
                  in)))) 

(defn part2 [in]
  )

;(core/do-parts part1 part2 input)