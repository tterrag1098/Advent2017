(ns advent-2017.day09)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

(def input (slurp "resources/day9.txt"))

;; A holder for the current state of the parsing:
;; str -> remaining string to parse
;; depth -> current amount of levels deep into the group nesting
;; score -> current total score of the parse
;; garbage -> accumulated string of all garbage characters
(defrecord State [ str depth score garbage ])

;; The default state
(def base-state (State. input 0 0 ""))

;; Returns a tuple of the next token, and the rest of the string
(defn token [state] [ (first (:str state)) (rest (:str state)) ])

;; Parse a single garbage (characters between <>)
(defn parse-garbage
  [state]
  (let [ [ nextchar after ] (token state) ] ; Bind next token and remaining text
    (case nextchar
      \> (assoc state :str after) ; Close the garbage and return
      \! (recur (assoc state :str (rest after))) ; Skip the following token and recurse
      (recur (update-in (assoc state :str after) [:garbage] str nextchar))))) ; Any other character, add it to the garbage string and recurse

(defn parse-group
  [state]
  (if (empty? (:str state)) state ; Base case, return if remaining string is empty
    (let [ [ nextchar after ] (token state) ; Bind next token and remaining text
             state (assoc state :str after) ] ; Overwrite state binding with remainder
      (case nextchar
        \{ (recur (update-in state [:depth] inc)) ; Open a new group, increase the depth and recurse
        \} (recur (update-in (update-in state [:score] + (:depth state)) [:depth] dec )) ; Close a group, add depth to score, decrease the depth, recurse
        \< (recur (parse-garbage state)) ; Open garbage, delegate to parse-garbage
        (recur state))))) ; Any other character (comma, newline) is skipped

(defn part1 [in]
  (:score (parse-group in)))

(defn part2 [in]
  (count (:garbage (parse-group in))))

(core/do-parts part1 part2 base-state)