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
  (if (nil? layer) layer
    (let [ size (:size layer)
           pos (:scanpos layer)
           fwd (:fwd layer) ]
      (->Layer size (if fwd (inc pos) (dec pos)) (get-direction size pos fwd)))))

(def sizes (apply sorted-map (map #(Integer/parseInt %) (flatten (map #(str/split % #":\s") input)))))

(def layers 
  (let [ ks (range (apply max (keys sizes))) ]
    (zipmap ks (map (fn [k]
                      (let [ v (sizes k) ]
                        (when (some? v) (->Layer v 0 true)))) ks))))

(defn advance
  ([layers] (advance layers 0 0))
  ([layers pos score]
    (if (> pos (apply max (keys layers))) score
      (let [ layer (layers pos) ]
        (recur
          (core/update-values layers tick)
          (inc pos)
          (if (and (some? layer) (= (:scanpos layer) 0)) (+ score (* (:size layer) pos)) score)))))) 

(defn part1 [in]
  (advance in))

(defn caught
  [sizes layer time]
    (let [ size (sizes layer) ]
      (when (some? size) (zero? (mod time (* 2 (- size 1)))))))

(defn safe
  ([sizes time] (safe sizes 0 time))
  ([sizes pos time]
  (if (> pos (apply max (keys sizes))) true
    (if (caught sizes pos time) false
      (recur sizes (inc pos) (inc time))))))
    
(defn part2 [in]
  ((fn [in delay]
     (let [ pass (safe in delay) ]
       (if pass delay
         (recur in (inc delay)))))
    in 0))

(core/do-parts part1 part2 layers sizes)