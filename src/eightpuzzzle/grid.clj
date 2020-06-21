(ns eightpuzzzle.grid)

;;////////////////////
(def ideal [[1 2 3] [4 5 6] [7 8 0]])
(def directions [:left :right :up :down])

;;////////////////////
(defn zero-finder                                           ;;finds zero in 3*3 vector
  [input]
  {:zero (first (for [i (range 3)
                      j (range 3)
                      :when (= 0 (get-in input [i j]))]
                  [i j]
                  ))})
(def zeo (last (:zero (zero-finder [[1 2 3] [4 5 6] [7 8 0]]))))
;;////////////////////
(defn solvable? [input]                                     ;;true if input was solvable
  (let [x (into [] (flatten input))]
    (let [reduced-check (reduce (fn [cnt head]              ;;check: vector of times head > whole x except 0
                                  (if (not= 0 head)
                                    (let [check (into [] (filter #(> head %) (filter #(not= 0 %) x)))]
                                      (conj cnt (count check)))
                                    cnt))
                                []
                                x)]
      (let [count-reduced-check (reduce + reduced-check)]
        (let [is-mod-odd? (even? count-reduced-check)]
          is-mod-odd?)))))
;////////////////////
(defn solvable? [x]
  (let [removed-zero (into [] (filter #(not= 0 %) x))]
    (loop [counts [] xs removed-zero]
      (if (seq xs)
        (let [head (first xs)
              tail (rest xs)
              counter (fn [accum curr] (if (> curr head) (inc accum) accum))]
          (recur (conj counts (reduce counter
                                      0
                                      tail)) tail))
        (even? (reduce + counts))))))
;;///////////////////
(defn swap-right                                            ;;ok if reaches border/ puts nil
  [x [i1 j1]]
  (let [f (get-in x [i1 j1])
        q (assoc-in x [i1 j1] (get-in x [i1 (+ 1 j1)]))
        s (assoc-in q [i1 (+ 1 j1)] f)]
    s))
(def testzero {:a 3 :b [1 1]})
(swap-right [[1 2 3] [4 5 6] [7 8 9]] (:b testzero))
(defn swap-left
  [x [i1 j1]]
  (let [f (get-in x [i1 j1])
        q (assoc-in x [i1 j1] (get-in x [i1 (- j1 1)]))
        s (assoc-in q [i1 (- j1 1)] f)]
    s))

(defn swap-down
  [x [i1 j1]]
  (let [f (get-in x [i1 j1])
        q (assoc-in x [i1 j1] (get-in x [(+ i1 1) j1]))
        s (assoc-in q [(+ 1 i1) j1] f)]
    s))

(defn swap-up
  [x [i1 j1]]
  (let [f (get-in x [i1 j1])
        q (assoc-in x [i1 j1] (get-in x [(- i1 1) j1]))
        s (assoc-in q [(- i1 1) j1] f)]
    s))
;;/////////////////
(defn count-same-elements
  [v1 v2]
  (let [x (loop [total-matches 0
                 [next-1 & more-1] v1
                 [next-2 & more-2] v2]
            (if-not (and next-1 next-2)
              total-matches
              (if (= next-1 next-2)
                (recur (inc total-matches) more-1 more-2)
                (recur total-matches more-1 more-2))))]
    x))
; (reduce + (map count-same-elements [[1 2 3] [4 5 6] [0 8 7]] ideal))
;;////////////////
(defn legal-moves [illegal]                                 ;;returns directions - illegal moves
  (let [legal (reduce (fn [legal x]
                        (if (some #(= x %) illegal)
                          legal
                          (conj legal x)))
                      []
                      directions)]
    legal))
;;////////////////
(defn init-input [input]
  {:vector input :history []})
;;////////////////
(defn move-reverse [move]
  (case move
    :up :down
    :right :left
    :left :right
    :down :up
    nil))

;;////////////////
;(defn out-of-boundry [zero]
;  (case zero
;    [0 0] [:up :left]
;    [0 2] [:up :right]
;    [2 2] [:down :left]
;    [2 0] [:down :right]))
;;//////////////////
(defn filter-false-val
  [m]
  (into {} (filter (fn [[k v]] (not= v false))
                   m)))
;;//////////////////





;;; (reduce + (map-indexed (fn [i val] (count (filter (partial > val) (drop i x)))) x)
