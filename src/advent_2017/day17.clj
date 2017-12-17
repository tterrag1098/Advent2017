(ns advent-2017.day17)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

(def input 345)
(def buffer [0])

(defn insert [vec pos item] (if (= 1 (count vec)) (conj vec item) (apply merge (subvec vec 0 (inc pos)) item (subvec vec (inc pos)))))
(defn advance [size cur amt] (mod (+ cur amt) size))

(defn step
  ([in jmp amt] (step in jmp amt 0 0))
  ([in jmp amt cur step]
    (if (zero? amt) in
     (let [ ins (advance (count in) cur jmp) ]
       (recur (insert in ins (inc step)) jmp (dec amt) (inc ins) (inc step))))))

(defn part1 [in]
  (let [ res (step buffer in 2017) ]
    (res (inc (.indexOf res 2017)))))

;; Simplified step, does not simulate list and only grabs a specific index from the result
(defn step-find
  ([jmp amt idx] (step-find jmp amt idx 0 0 nil))
  ([jmp amt idx cur step found]
    (if (zero? amt) found
      (let [ ins (advance (inc step) cur jmp) ]
        (recur jmp (dec amt) idx (inc ins) (inc step) (if (= idx (inc ins)) (inc step) found))))))

(defn part2 [in] (step-find in 50000000 1))

(core/do-parts part1 part2 input)