(ns advent-2017.day23)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

(defn parse-reg [s] (if (Character/isLetter (first s)) (first s) (Integer/parseInt s)))

(def input 
  (vec (map (fn [line]
              (-> line
                  (str/split #"\s")
                  (update-in [1] parse-reg)
                  (update-in [2] parse-reg)))
            (core/read-input "day23.txt"))))

(defn reg [registers k] (if (number? k) k (registers k 0)))

(defn exec [registers instr pos]
  (let [ [ op x y ] instr ]
    (-> op
        (case
          "set" (assoc registers x (reg registers y))
          "sub" (assoc registers x (- (reg registers x) (reg registers y)))
          "mul" (assoc registers x (* (reg registers x) (reg registers y)))
          registers)
        (vector (if (and (= op "jnz") (not= 0 (reg registers x))) (dec (+ pos (reg registers y))) pos)))))

(defn run 
  ([instrs] (run {} instrs 0 0))
  ([registers instrs pos mulc]
    (if (>= pos (count instrs)) mulc
      (let [ instr (instrs pos)
             res (exec registers instr pos) ]
        (recur (first res) instrs (inc (second res)) (if (= "mul" (first instr)) (inc mulc) mulc))))))
  
(defn part1 [in]
  (run in))

(defn part2 [in]
  )

;(core/do-parts part1 part2 input)