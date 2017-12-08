(ns advent-2017.day08)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

;; Split up input by lines then by whitespace, and throw out the "if"
(def input (map #(str/split % #"\s(if\s)?") (core/read-input "day8.txt")))

;; Map opcodes to their mathematical functions 
(def opcodes { "inc" + "dec" - })

;; Map comparisons to their boolean functions
(def cmprs { ">" > "<" < ">=" >= "<=" <= "==" == "!=" not= })

;; Better than a raw list (kind of?)
(defrecord Instruction [register opcode amt condA cmpr condB])

;; Map the parsed strings to records
(def instructions (map #(apply ->Instruction %) input))

;; Get a value from the registers, 0 if register is not present
(defn reg-val [registers reg]
  (let [ val (registers reg) ]
    (if (nil? val) 0 val)))

;; Find the maximum value in any register
(defn max-register [registers] (if (empty? registers) 0 (val (apply max-key val registers))))

;; Executes the instructions, returning a map with the resultant registers, and the max value seen (for part 2)
(defn run-code
  [instructions registers maxval]
  (if (empty? instructions) { :registers registers :max maxval } ; Base case
	  (let [ instr (first instructions) ; Grab the first instruction and extract/parse its data
	         target (:register instr)
	         opcode (opcodes (:opcode instr))
	         amt (Integer/parseInt (:amt instr))
	         condA (:condA instr)
	         cmpr (cmprs (:cmpr instr))
	         condB (Integer/parseInt (:condB instr)) ]
	    (recur (rest instructions) ; Pass up the rest of the instructions
	           (if (cmpr (reg-val registers condA) condB) ; If the comparison passes
               (assoc registers target (opcode (reg-val registers target) amt)) ; Update the value in the register
               registers) ; Otherwise do nothing
             (max (max-register registers) maxval))))) ; Pass the new max if necessary

;; Part 1, just extract the max register from the resultant registers
(defn part1 [in] (max-register ((run-code in {} 0) :registers)))

;; Part 2, just read the :max keyword from the result of run-code
(defn part2 [in] ((run-code in {} 0) :max))

(core/do-parts part1 part2 instructions)