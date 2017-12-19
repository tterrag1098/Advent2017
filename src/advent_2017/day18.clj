(ns advent-2017.day18)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

(def input (map (fn [l] (map #(if (re-matches #"[a-z]+" %) % (Integer/parseInt %)) l)) (map #(str/split % #"\s") (core/read-input "day18.txt"))))

(defn reg [registers input] (if (number? input) input (registers (first input) 0)))

(defrecord State [registers prevfreq pos snds])

;; Apply a function to a register
(defn transform-reg [regs args f]
  (assoc regs (ffirst args) (f (regs (ffirst args) 0) (reg regs (second args)))))

;; Perform a single instruction
(defn exec-one
  [state instr]
   (let [ regs (:registers state) 
          prevf (:prevfreq state)
          pos (inc (:pos state))
          snds (:snds state)
          args (rest instr) 
          r (first args)
          v (when (= 2 (count args)) (reg regs (second args))) ]
     (case (first instr)
       "snd" (->State regs (reg regs r) pos (inc snds))
       "set" (->State (assoc regs (first r) v) prevf pos snds)
       "add" (->State (transform-reg regs args +) prevf pos snds)
       "mul" (->State (transform-reg regs args *) prevf pos snds)
       "mod" (->State (transform-reg regs args mod) prevf pos snds)
       "rcv" (if (zero? (reg regs r)) state (->State (assoc regs (first r) prevf) prevf pos snds))
       "jgz" (if (pos? (reg regs r)) (->State regs prevf (dec (+ pos v)) snds) (->State regs prevf pos snds)))))

;; Runs a set of instructions to completion, using a bit of a hack to detect infinite looping
(defn exec
  ([instructions] (exec instructions (->State {} nil 0 0)))
  ([instructions state] (exec instructions state nil))
  ([instructions state prev] ;; Keep the previous instructions so that we can compare every other state
    (let [ res (exec-one state (nth instructions (:pos state))) 
           next (:pos res) ]
      ; If this is the same as the state 2 steps ago, we are probably looping infinitely, so return
      (if (or (= res prev) (< next 0) (>= next (count instructions))) res
        (recur instructions res state)))))

(defn part1 [in]
  (:prevfreq (exec in)))

;; Redefining the above logic in exec-one for part 2, copypasta galore
(defn exec-q [instr state regs args snds r v pos qin qout]
  (let [ pos (inc pos) ]
    (case (first instr)
      "snd" [(->State regs nil pos (inc snds)) qin (conj qout (reg regs r))]
      "set" [(->State (assoc regs (first r) v) nil pos snds) qin qout]
      "add" [(->State (transform-reg regs args +) nil pos snds) qin qout]
      "mul" [(->State (transform-reg regs args *) nil pos snds) qin qout]
      "mod" [(->State (transform-reg regs args mod) nil pos snds) qin qout]
      "rcv" (if (empty? qin) [state qin qout] [(->State (assoc regs (first r) (peek qin)) nil pos snds) (pop qin) qout])
      "jgz" [(if (pos? (reg regs r)) (->State regs nil (dec (+ pos v)) snds) (->State regs nil pos snds)) qin qout])))

;; Executes two programs "simultaneously"
;; In reality, if a program calls rcv on an empty queue, it will busy wait and poll the queue every time,
;; staying on the exact same state until a value can be popped. There is no concurrency.
(defn exec-two
  ([instrs] (exec-two instrs (->State {\p 0} nil 0 0) clojure.lang.PersistentQueue/EMPTY (->State {\p 1} nil 0 0) clojure.lang.PersistentQueue/EMPTY))
  ([instrs state1 q1 state2 q2]
    ; Could I probably use a threading macro for this? Maybe. 
    (let [ [regs1 regs2 :as regs] (map :registers [state1 state2]) 
           [pos1 pos2 :as pos] (map :pos [state1 state2])
           [i1 i2] [(nth instrs pos1) (nth instrs pos2)]
           [args1 args2 :as args] (map rest [i1 i2]) 
           [r1 r2 :as r] (map first args)
           [v1 v2] (map-indexed #(when (= 2 (count %2)) (reg (nth regs %1) (second %2))) args)
           res1 (exec-q i1 state1 regs1 args1 (:snds state1) r1 v1 pos1 q1 q2)
           [q1 q2] (rest res1)
           res2 (exec-q i2 state2 regs2 args2 (:snds state2) r2 v2 pos2 q2 q1)
           [q2 q1] (rest res2) ] ; Important! These two are reversed!
      ; If both states are identical to their predecessor, we are in a deadlock, so return!
      (if (and (= (first res1) state1) (= (first res2) state2)) (map first [res1 res2])
        (recur instrs (first res1) q1 (first res2) q2)))))

(defn part2 [in]
  (:snds (second (exec-two in))))

(core/do-parts part1 part2 input)