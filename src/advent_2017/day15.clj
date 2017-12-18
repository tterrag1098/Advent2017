(ns advent-2017.day15)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

(def state [722 354])
(def factors [16807 48271])

(defn generate 
  ([state] (map-indexed #(generate %2 (nth factors %1)) state))
  ([val factor] (mod (* factor val) 2147483647)))
  
(defn accept [state] (apply == (map #(bit-and % 0xFFFF) state)))
(defn generator [in factor pred] (let [ in (generate in factor) ] (if (pred in) in (recur in factor pred))))

(defn simulate 
  ([state preds amt ] (simulate state preds amt 0))
  ([state preds amt cnt ]
    (if (zero? amt) cnt
      (let [ state (map-indexed #(generator %2 (nth factors %1) (nth preds %1)) state) ]
        (recur state preds (dec amt) (if (accept state) (inc cnt) cnt))))))

(defn part1 [in] (simulate in [some? some?] 40000000))
(defn part2 [in] (simulate in [#(zero? (mod % 4)) #(zero? (mod % 8))] 5000000))

(core/do-parts part1 part2 state)