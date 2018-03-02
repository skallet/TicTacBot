(ns tictactoe.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [tictactoe.draw :as draw]
            [tictactoe.bot :as bot]
            [tictactoe.board :as board]))

(def game
 {:size {:width 300 :height 300}
  :cell {:size 25}
  :win-count 5})

(defn create-initial-state
 [game]
 (let [size (:size game)
       cell-size (:size (:cell game))
       rows (/ (:height size) cell-size)
       cols (/ (:width size) cell-size)
       board-row (transduce (take cols) conj (repeat ::board/EMPTY))
       board (transduce (take rows) conj (repeat board-row))]
   {:rows rows
    :cols cols
    :board board
    :game game
    :status ::board/PLAY
    :player ::board/CIRCLE}))

(defn setup []
  (q/frame-rate 24)
  (q/color-mode :rgb)
  (create-initial-state game))

(defn get-next-player
  [{player :player status :status}]
  (when (= status ::board/PLAY)
    (if (= player ::board/CIRCLE) ::board/CROSS ::board/CIRCLE)))

(defn update-next-player
  [state]
  (let [next-player (get-next-player state)]
    (assoc state :player next-player)))

(defn update-game-status
  [state]
  (let [player (:player state)
        won (board/get-winning-cells player state)]
    (assoc state :status (if won ::board/FINISHED ::board/PLAY))))

(defn make-move
  [state x y player]
  (let [board (:board state)
        current (:player state)]
    (if (= current player)
      (->
        state
        (assoc :board (board/update-cell x y player board))
        (update-game-status)
        (update-next-player))
      state)))


(defn update-state [state]
  (let [player (:player state)
        move (when (= player ::board/CROSS) (bot/make-move state))]
    (if
      move
      (make-move state (nth move 0) (nth move 1) player)
      state)))

(defn on-mouse-clicked
  [state {x :x y :y button :button}]
  (let [board (:board state)
        cell (:cell (:game state))
        cell-x (quot x (:size cell))
        cell-y (quot y (:size cell))
        player (:player state)
        status (:status state)
        board-cell (board/get-cell cell-x cell-y board)]
    (if (and
          (= player ::board/CIRCLE)
          (= button :left)
          (= board-cell ::board/EMPTY)
          (= status ::board/PLAY))
      (->
        state
        ; (assoc :player ::CROSS)
        (make-move cell-x cell-y player))
      state)))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  ; (println state)
  (q/background 240)
  (q/stroke-weight 1)
  (q/stroke 0 0 0)
  (draw/draw-lines (board/get-row-lines state))
  (draw/draw-lines (board/get-col-lines state))
  (draw/draw-cells state)
  (let [board (:board state)
        win-circle (board/get-winning-cells ::board/CIRCLE state)
        win-cross (board/get-winning-cells ::board/CROSS state)
        win (or win-circle win-cross)]
    (when (not (nil? win)) (draw/draw-win win state))))


(q/defsketch tictactoe
  :title "TicTacToe"
  :size [(:width (:size game)) (:height (:size game))]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :mouse-clicked on-mouse-clicked
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
