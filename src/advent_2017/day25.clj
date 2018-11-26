(ns advent-2017.day25)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

(def start \A)
(def steps 12629077)

(def input
  { \A { 0 [1 1 \B] 1 [0 -1 \B] }
    \B { 0 [0 1 \C] 1 [1 -1 \B] }
    \C { 0 [1 1 \D] 1 [0 -1 \A] }
    \D { 0 [1 -1 \E] 1 [1 -1 \F] }
    \E { 0 [1 -1 \A] 1 [0 -1 \D] }
    \F { 0 [1 1 \A] 1 [1 -1 \E] } })

(defrecord State [pos state])

(defn transition [state code tape amt]
  (if (zero? amt) tape
    (let [ pos (:pos state) 
           instr ((code (:state state)) (tape pos 0))
           val (first instr)
           lr (second instr)
           newstate (last instr) ]
      (recur (->State (+ pos lr) newstate) code (assoc tape pos val) (dec amt)))))

(defn part1 [in]
  (apply + (vals (transition (->State 0 start) in {} steps)))) 

(defn part2 [in]
  )

;(core/do-parts part1 part2 input)