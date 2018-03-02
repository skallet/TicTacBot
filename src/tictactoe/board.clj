(ns tictactoe.board
  (:require [quil.core :as q]))

(defn get-boarder-lines
  [game count map-to-line]
  (let [cell-size (:size (:cell game))]
    (->>
     count
     (range 1)
     (map #(* cell-size %))
     (map map-to-line))))

(defn get-row-lines
 [{rows :rows game :game}]
 (get-boarder-lines game rows #(list 0 % (:width (:size game)) %)))

(defn get-col-lines
 [{cols :cols game :game}]
 (get-boarder-lines game cols #(list % 0 % (:height (:size game)))))

(defn get-cell
  [x y board]
  (->
    board
    (nth y)
    (nth x)))

(defn get-empty-cells
  [board]
  (for [[y row] (map-indexed vector board)
        [x val] (map-indexed vector row)
        :when (= ::EMPTY val)]
      [x y val]))

(defn get-non-empty-cells
  [board]
  (for [[y row] (map-indexed vector board)
        [x val] (map-indexed vector row)
        :when (not= ::EMPTY val)]
      [x y val]))

(defn get-cells-type
  [type board]
  (->>
    board
    (get-non-empty-cells)
    (filter (fn [[x y val]] (= val type)))))

(defn update-cell
  [x y new-cell board]
  (assoc-in board [y x] new-cell))

(defn seqs-by-coord
  [n]
  (fn
    [seqs cell]
    (let [last-seq (last seqs)
          last-el (last last-seq)]
      (cond
        (nil? last-seq) [[cell]]
        (= (nth cell n) (+ 1 (nth last-el n))) (conj (drop-last seqs) (conj last-seq cell))
        :else (conj seqs [cell])))))

(defn group-and-find-seqs
  [cells group-n coord-n]
  (->>
    cells
    (group-by #(nth % group-n))
    (vals)
    (into [])
    (map
      #(reduce (seqs-by-coord coord-n) [] %))
    (reduce concat)))

(defn find-horizontal-sequences
  [cells]
  (group-and-find-seqs cells 1 0))

(defn find-vertical-sequences
  [cells]
  (group-and-find-seqs cells 0 1))

(defn remap-cells
  [cells map-fn]
  (map
   (fn [[x y cell]]
     [x y cell (map-fn x y) y])
   cells))

(defn find-diagonal-l-sequences
  [cells]
  (let [mapped (remap-cells cells +)]
    (group-and-find-seqs mapped 3 1)))

(defn find-diagonal-r-sequences
  [cells]
  (let [mapped (remap-cells cells -)]
    (group-and-find-seqs mapped 3 1)))

(defn get-winning-cells
  [type board]
  (let [cells (get-cells-type type board)
        vertical (find-vertical-sequences cells)
        horizontal (find-horizontal-sequences cells)
        diagonal-l (find-diagonal-l-sequences cells)
        diagonal-r (find-diagonal-r-sequences cells)
        all (concat vertical horizontal diagonal-l diagonal-r)]
    (first
      (filter #(>= (count %) 5) all))))
