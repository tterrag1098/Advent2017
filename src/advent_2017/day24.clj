(ns advent-2017.day24)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

(def input (map (fn [c] (vec (map #(Integer/parseInt %) (str/split c #"/")))) 
                ["0/2"
"2/2"
"2/3"
"3/4"
"3/5"
"0/1"
"10/1"
"9/10"]));(core/read-input "day24.txt")))

(defrecord Component [ports free])
(defn new-comp [ports] (->Component ports [true true]))

(def components (apply hash-set (map new-comp input)))
(defn get-roots [components] (apply hash-set (filter #(some #{0} (:ports %)) components)))

(defn get-avail [comp]
  (if (first (:free comp)) (first (:ports comp)) (second (:ports comp))))

(defn mark-used [comp val]
  (->Component (:ports comp) (vec (map-indexed #(and %2 (not= (nth (:ports comp) %1) val)) (:free comp)))))

(defn component-has [comp val]
  (let [ avail (keep-indexed #(when (nth (:free comp) %1) %2) (:ports comp)) ]
    (some? (some #{val} avail))))

(defn compatible [comp next]
  (let [ needs (get-avail comp) ]
    (component-has next needs)))

(defn strength [path]
  (apply + (map #(apply + (:ports %)) path)))

(defn find-bridge 
  ([components]
    (->> components
          get-roots
          (map #(find-bridge (disj components %) %))))
  ([components root] 
    (let [ root (->Component (:ports root) [(pos? (first (:ports root))) (pos? (second (:ports root)))]) ]
      (find-bridge components (list root) [] [])))
  ([components search path strongest]
    (do (prn strongest) (prn components) (if (empty? search) strongest
      (let [ next (peek search)
             compat (->> components
                         (filter #(and (not= %1 next) (compatible next %1)))
                         (map #(mark-used % (get-avail next)))) 
             search (if (empty? compat) (pop search) (apply conj (pop search) compat)) ]
        (recur 
          (filter (fn [x] (empty? (filter #(= (:ports x) (:ports %)) path))) components) 
          search
          (if (empty? compat) (if (empty? search) path (pop path)) (conj path next))
          (max-key strength path strongest)))))))

(defn part1 [in]
  (apply max (map strength (find-bridge in))))

(defn part2 [in]
  )

;(core/do-parts part1 part2 input)