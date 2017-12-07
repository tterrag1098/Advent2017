(ns advent-2017.day07)
(require '[advent-2017.core :as core])

;; Matcher for input, group 1 = name, group 2 = weight, group 3 = holding (if any)
(def matcher #(re-matcher #"([a-z]+)\s\(([0-9]+)\)(?:\s->\s([a-z,\s]+))?" %))

(def input (core/read-input "day7.txt"))

;; Parse the input with the matcher and construct a map of data for each entry
(def data
  (vec (let [ parsed (map #(re-find (matcher %)) input) ] ; Hold the array of group data
         (map (fn [match] ; Map each match, putting the groups into a map
                (let [ name (nth match 1)
                      weight (Integer/parseInt (nth match 2))
                      holding (nth match 3)
                      ; Assign the map pairs
                      res { :name name :weight weight :holding (when (some? holding) (set (str/split holding #",\s"))) } ]
                  (if (nil? (res :holding)) (dissoc res :holding) res))) ; Clear out nil holding
             parsed))))

;; Given the tree, a tree element, and the new parent for this element,
;; add the element as a child of the parent and return the modified tree
(defn sort-in
  [in data parent]
  (filter
    #(not= (% :name) (data :name)) ; Remove the element
    (assoc 
      (vec in) ; Make sure to un-lazy
      (core/first-idx #(= (% :name) (parent :name)) in) ; Find the parent's index in the list
      (let [sorted (assoc parent ; Modify the parent, appending the new child and removing the child's name from the "holding" set
                          :children (conj (parent :children) data) 
                          :holding (set (remove #(= % (data :name)) (parent :holding)))) ]
        (if (empty? (sorted :holding)) (dissoc sorted :holding) sorted))))) ; Clear out empty holding sets

;; Perform sort-in recursively until a tree structure is formed
(defn heapify
  [in]
  (let [ to-sort (first (filter #(empty? (% :holding)) in)) ; Find the first element that is holding nothing
         ; Find its parent, which is the element that has the child's name in its holding set
         parent (when (some? to-sort) (first (filter #(contains? (% :holding) (to-sort :name)) in))) ]
    (if (nil? parent) ; If the parent doesn't exist, this tree is complete
      in
      (recur (sort-in in to-sort parent))))) ; Add this child to the tree structure and recurse

;; Heapify leaves us with a 1-size list, so extract that
(def root (nth (heapify data) 0))

;; Makes it look easy, doesn't it?
(def part1 :name)

;; Find the total weight of the given element, which is the sum of its weight and all its childrens' weights recursively
(defn total-weight [in]
  (+ (in :weight) (apply + (map total-weight (in :children)))))

;; Recursively iterate the tree and add 'tot-weight' entries to every element
(defn sum-weights [in]
  (if (empty? (in :children)) in
	  (let [ updated (assoc in :tot-weight (total-weight in)) ]
	    (assoc updated :children (map sum-weights (updated :children))))))

;; Recursive helper for find-problem, as we must actually return the *parent* of the problem element 
(defn find-problem-r [in parent]
  (if (apply = (map :tot-weight (in :children))) parent
    (recur (apply max-key :tot-weight (in :children)) in)))

;; Finds the parent of the "problem" element in the tree, i.e. the one with an invalid weight 
(defn find-problem [in] (find-problem-r in nil))

(defn part2 [in]
  (let [ problem (find-problem (sum-weights in)) ; Find the parent node of the problem
         bad-child (apply max-key :tot-weight (problem :children)) ; We know the problem will always be too heavy, so extract the child with the max weight
         child-weights (map :tot-weight (problem :children)) ] ; Construct a convenience list of the child total weights
    ; The result is the "bad" child's weight minus the difference between the max and min total weights of all children
    (- (bad-child :weight) (- (apply max child-weights) (apply min child-weights)))))

(core/do-parts part1 part2 root)