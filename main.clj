(ns mfqclib
    (:use clojure.pprint))

(defn empty-board
  "Creates a rectangular empty board of the specified width and height"
  [w h]
  (vec (repeat w (vec (repeat h nil))))
)

(defn populate
  "Turns :on each of the cells specificed as [x,y] coordiantes."
  [board living-cells]
  (reduce (fn [board coordinates]
            (assoc-in board coordinates :on))
          board
          living-cells)
)

(defn neighbours
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)])


(defn count-neighbours
  [board loc]
  (count (filter #(get-in board %) (neighbours loc)))
)

(defn livenessf
  "An attempt to understand the liveness function"
  [board loc]
  (case (count-neighbours board loc)
    2 (get-in board loc)
    3 :on
    nil)
)

(defn indexed-step
  "Yields the next state of the board using indicies to determine neighbours, liveness, etc"
  [board]
  (let [w (count board)
       h (count (first board))]
    (loop [new-board board x 0 y 0]
      (cond
        (>= x w) new-board
        (>= y h) (recur new-board (inc x) 0)
        :else
          (let [new-liveness
                (case (count-neighbours board [x y])
                  2 (get-in board [x y])
                  3 :on
                  nil)]
            (recur (assoc-in new-board [x y] new-liveness) x (inc y))
          )
      )
    )
  )
)


(def glider (populate (empty-board 6 6) #{[2 0] [2 1] [2 2] [1 2] [0 1] }))

(-> (iterate indexed-step glider) (nth 8) pprint)





