(ns tictactoe.bot
  (:require [tictactoe.board :as board]))

(defn make-move
  [{board :board player :player}]
  (let [empty-cells (board/get-empty-cells board)
        random-empty-cell (first (shuffle empty-cells))]
    random-empty-cell))
