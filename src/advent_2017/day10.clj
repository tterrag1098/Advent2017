(ns advent-2017.day10)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

;; The data to hash, a range of numbers from 0->255
(def data (vec (range 256)))

;; Puzzle input
(def input "197,97,204,108,1,29,5,71,0,50,2,255,248,78,254,63")
;; The input for part 1, just simply the parsed numbers
(def input1 (map #(Integer/parseInt %) (str/split input #",")))

;; Part 2's extra lengths
(def extralengths [17 31 73 47 23])
;; The input for part 2, convert each character in the input to bytes and then append the "extra lengths" from the problem
(defn get-lengths
  [in]
  (apply conj (vec (map byte in)) extralengths))

;; Get a wrapped sublist from the data
(defn sublist
  [in start len]
  (if (zero? len) [] ; Short circuit 0-length
    (let [ end (mod (+ start len) (count in)) ]
      (if (<= end start) ; If the end is <= the start, the sublist needs to be "wrapped" around the end of the list
        (vec (concat (subvec in start) (subvec in 0 end))) ; Combine the sublist from start->last and the sublist from first->end
        (vec (subvec in start end))))))

;; Replace a sublist into the data
(defn replace-sub
  [in start sub]
  (if (empty? sub) in
    (recur (assoc in start (first sub)) (mod (inc start) (count in)) (rest sub))))

;; Return the sublist from start to (start + len), wrapped, and reversed
(defn reverse-sub
  [in start len]
  (replace-sub in start (reverse (sublist in start len))))

;; Performs a single hash, returning the state of the hashing at time of completion
(defn do-hash-single
  ([in lengths cur skip]
    (if (empty? lengths) { :in in :cur cur :skip skip } ; Base case, return current state
      (let [ len (first lengths) ]
        (recur (reverse-sub in cur len) (rest lengths) (mod (+ cur len skip) (count in)) (inc skip))))))

;; Perform a hash, optionally with a number of rounds
(defn do-hash
  ([in lengths] (do-hash in lengths 1))
  ([in lengths rounds] (do-hash in lengths 0 0 rounds))
  ([in lengths cur skip rounds]
    (if (zero? rounds) in ; Base case, no rounds left
      (let [ res (do-hash-single in lengths cur skip) ] ; Do a single hash, store the result state
        (recur (res :in) lengths (res :cur) (res :skip) (dec rounds)))))) ; Recurse with the single-hash's result state

(defn part1 [in]
  (let [ res (do-hash data in) ]
    (* (first res) (second res))))

;; Reduce the resultant 256-size vector to a 16-size vector of XOR'd values
(defn dense-hash [in] (map #(reduce bit-xor %) (partition 16 in)))
;; Combine the XOR'd values into a hexadecimal string
(defn hash-str [in] (.toLowerCase (apply str (map #(format "%02X" %) in))))

(defn part2 [in] (hash-str (dense-hash (do-hash data in 64))))

(core/do-parts part1 part2 input1 (get-lengths input))