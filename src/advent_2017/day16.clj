(ns advent-2017.day16)
(require '[clojure.string :as str])
(require '[advent-2017.core :as core])

(def input (str/split (str/trim (slurp "resources/day16.txt")) #","))

(def programs (vec (map char (range (int \a) (inc (int \p))))))

(defn spin [p x]
  (let [ piv (- (count p) x)
         sub (subvec p piv)
         rest (subvec p 0 piv) ]
    (vec (concat sub rest))))

(defn exch [p a b] (vec (assoc p a (p b) b (p a))))
(defn part [p a b] (exch p (.indexOf p a) (.indexOf p b)))

(defn dance [p steps]
  (if (empty? steps) p
    (let [ instr (first steps) ]
      (case (first instr)
        \s (recur (spin p (Integer/parseInt (subs instr 1))) (rest steps))
        \x (recur (apply exch p (map #(Integer/parseInt %) (str/split (subs instr 1) #"/"))) (rest steps))
        \p (recur (apply part p (map first (str/split (subs instr 1) #"/"))) (rest steps))))))

(defn part1 [in]
  (apply str (dance programs in)))

(defn first-loop
  ([p steps] (first-loop p steps 0))
  ([p steps acc]
    (if (and (> acc 0) (= p programs)) acc
      (recur (dance p steps) steps (inc acc)))))

(def cycles 1000000000)

(defn part2 [in]
  (let [ loop (first-loop programs in)
         amt (mod cycles loop) ]
    (apply str ((fn [p steps amt]
                  (if (zero? amt) p
                    (recur (dance p steps) steps (dec amt))))
                 programs in amt))))

(core/do-parts part1 part2 input)