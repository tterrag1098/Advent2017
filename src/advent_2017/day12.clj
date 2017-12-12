(ns advent-2017.day12)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

(def input (core/read-input "day12.txt"))
;; Split up the input by the <->, and turn it into a string->[string] map
(def parsed (apply conj (map #(hash-map (first %) (str/split (second %) #",\s")) (map #(str/split % #"\s<->\s") input))))
;; Zip up the parsed integer values into a map, which represents the pipes
(def pipes (zipmap (map #(Integer/parseInt %) (keys parsed)) (map (fn [v] (map #(Integer/parseInt %) v)) (vals parsed))))

;; Basic BFS impl
(defn bfs 
  ([g root] (bfs g (list root) #{}))
  ([g search found]
    (if (empty? search) found ;; No more nodes left to search
      (let [ next (peek search) ;; Pop first node off the search queue
             edges (filter #(not (contains? found %)) (g next)) ] ;; Find the edges of this node which have not already been found
        (if (empty? edges)
          (recur g (pop search) (conj found next)) ;; No new edges, just pop search and add node to seen
          (recur g (apply conj (pop search) edges) (conj found next))))))) ;; Add children of next to the search queue

;; Part 1 is just the count of nodes which can be found from 0
(defn part1 [in]
  (count (bfs in 0)))

;; For part 2, recursively process the pipes, each time removing the nodes found by the previous iteration
(defn part2 [in]
  ((fn [in groups]
     (if (empty? in) groups
       (let [ res (bfs in (ffirst in)) ]
         (recur (apply dissoc in res) (inc groups)))))
    in 0))

(core/do-parts part1 part2 pipes)