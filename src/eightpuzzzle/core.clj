(ns eightpuzzzle.core
  (:gen-class)
  (:use eightpuzzzle.grid))

;;///////////////////////////////////
(defn main [init]
  (if (solvable? (into [] (flatten init)))
    (let [input {:vector init :history []}]
      (loop [i 0 node input]
        ; (println "initial input" node)
        (if (or (= ideal (:vector node))
                (> i 1000))
          (conj (:history node) "End")

          (let [zero (:zero (zero-finder (:vector node)))]
            (let [moves {:right (if (or (= 2 (last zero)) (= :right (move-reverse (last (:history node)))))
                                  false
                                  (do
                                    ;(println "zero: " zero)
                                    ; (println "before swap: " (:vector node))
                                    (swap-right (:vector node) zero)))
                         :left  (if (or (= 0 (last zero)) (= :left (move-reverse (last (:history node)))))
                                  false
                                  (swap-left (:vector node) zero))
                         :up    (if (or (= 0 (first zero)) (= :up (move-reverse (last (:history node)))))
                                  false
                                  (swap-up (:vector node) zero))
                         :down  (if (or (= 2 (first zero)) (= :down (move-reverse (last (:history node)))))
                                  false
                                  (swap-down (:vector node) zero))
                         }]

              (let [moves-cost {:right (if (= false (:right moves))
                                         false
                                         (reduce + (count (:history node)) (map count-same-elements (:right moves) ideal)))
                                :left  (if (= false (:left moves))
                                         false
                                         (reduce + (count (:history node)) (map count-same-elements (:left moves) ideal)))
                                :up    (if (= false (:up moves))
                                         false
                                         (reduce + (count (:history node)) (map count-same-elements (:up moves) ideal)))
                                :down  (if (= false (:down moves))
                                         false
                                         (reduce + (count (:history node)) (map count-same-elements (:down moves) ideal)))
                                }]

                (let [next-node-key (key (apply max-key val (filter-false-val moves-cost)))]
                  (let [next-node-vector (get moves next-node-key)]
                    (recur (inc i) {:vector next-node-vector :history (conj (:history node) next-node-key)})
                    ))))))))))

(main [[2 3 4] [1 5 0] [7 6 8]])
(into [] (flatten [[1 2 3] [4 5 6] [[7 0 8]]]))

(def xs [{:title "Title1" :id 18347125}
               {:title "Title2" :id 18347123}
               {:title "Title3" :id 18341121}])
(some #(= (= "title1" (:title xs)) %) xs)

(->> xs
     (filter #(= (:id %) 183247125))
     first)

