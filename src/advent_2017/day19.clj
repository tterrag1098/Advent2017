(ns advent-2017.day19)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

(def input (core/read-input "day19.txt"))

;; Holder for the current walk state
(defrecord State [x y dir seen steps])
;; Find the start position, which is simply the only | character on the first line
(def start (->State (.indexOf (first input) "|") 0 1 [] 0))

;; Get a char from the graph, returning a space if out of bounds
(defn get-char [graph x y]
  (let [ row (some-> graph (nth y)) ]
    (if (and (< y (count row)) (>= y 0)) (nth row x) \ )))

;; Compute the direction (0-3 -> UDRL) from two positions
(defn get-dir [x1 y1 x2 y2]
  (if (< y2 y1) 0 (if (> y2 y1) 1 (if (> x2 x1) 2 3))))

;; Move the coordinate the given direction
(defn move [x y dir]
  (case dir
    0 [x (dec y)]
    1 [x (inc y)]
    2 [(inc x) y]
    3 [(dec x) y]))

;; Turn the current state, used when a + char is encountered
(defn turn [graph state]
  (let [ [x y] [(:x state) (:y state)] ]
    (as-> state $
      (< (:dir $) 2) ; Compute the axis, true for Y, false for X
      (if $ [[(inc x) y][(dec x) y]] [[x (inc y)][x (dec y)]]) ; Compute next position candidates based on axis
      (into {} (map #(vector (apply get-char graph %) %) $)) ; Convert to a map of char->pos
      (first (select-keys $ [\| \-])) ; Get the first (should only be one) value that is a valid move
      ; Update the state with the new position and direction, and increment steps taken             
      (let [[nx ny] (val $)] (->State nx ny (apply get-dir x y (val $)) (:seen state) (inc (:steps state)))))))

;; Advance the walk by one step
(defn advance [graph state]
  (let [ cur (get-char (:x state) (:y state)) ]
    (case cur
      \  state ; A space character was found at the end of a path with no +, this is the end!
      \+ (recur graph (turn graph state)) ; Found a +, do a turn
      ; Update the state, conj the current char to the seen set if it's not | or -, and move xy along the current dir
      (let [ seen (as-> (:seen state) s (if (or (= \| cur) (= \- cur)) s (conj s cur))) 
             [x y] (move (:x state) (:y state) (:dir state)) ] 
        (->State x y (:dir state) seen (inc (:steps state)))))))

;; Perform advance recursively, returning the state if nothing changes (hit the end)
(defn walk [graph state]
  (let [ next (advance graph state) ]
    (if (= next state) state
      (recur graph next))))

(let [res (walk input start) ]
  (core/print-parts (apply str (:seen res)) (:steps res)))