(ns advent-2017.day13)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

(def input (core/read-input "day13.txt"))

(defn get-direction
  [size pos dir]
  (if (and dir (= pos (- size 2)))
    false
    (if (and (not dir) (= pos 1))
      true
      dir)))

(defrecord Layer [size scanpos fwd])

(defn tick [layer]
  (let [ size (:size layer)
         pos (:scanpos layer)
         fwd (:fwd layer) ]
    (->Layer size (if fwd (inc pos) (dec pos)) (get-direction size pos fwd))))

(def sizes (apply sorted-map (map #(Integer/parseInt %) (flatten (map #(str/split % #":\s") input)))))

(def layers 
  (let [ ks (range (apply max (keys sizes))) ]
    (zipmap ks (map (fn [k]
                      (let [ v (sizes k) ]
                        (when (some? v) (->Layer v 0 true)))) ks))))

(defn part1 [in]
  )

(defn part2 [in]
  )

(core/do-parts part1 part2 input)