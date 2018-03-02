(ns tictactoe.draw
  (:require [quil.core :as q]
            [tictactoe.board :as board]))

(defn draw-line
  [line]
  (apply q/line line))

(defn draw-lines
  [lines]
  (doall
    (map draw-line lines)))

(defn draw-circle
  [x y size]
  (let [translate (fn [x] (+ (/ size 2) (* size x)))
        spacing (* 0.20 size)
        real-x (translate x)
        real-y (translate y)
        real-size (- size spacing)]
    (q/no-fill)
    (q/stroke 0 0 255)
    (q/stroke-weight 5)
    (q/ellipse real-x real-y real-size real-size)))

(defn draw-cross
  [x y size]
  (let [spacing (* 0.20 size)
        translate (fn [x] (+ spacing (* size x)))
        x0 (translate x)
        y0 (translate y)
        x1 (- (+ x0 size) (* 2 spacing))
        y1 (- (+ y0 size) (* 2 spacing))]
    (q/stroke 255 0 0)
    (q/stroke-weight 5)
    (draw-lines [(list x0 y0 x1 y1) (list x1 y0 x0 y1)])))

(defn pick-drawer
  [type]
  (cond
    (= type ::board/CIRCLE) draw-circle
    :else draw-cross))

(defn draw-cell
  [[x y type] size]
  (let [draw-type (pick-drawer type)]
    (draw-type x y size)))

(defn draw-cells
  [{board :board game :game}]
  (let [non-empty-cells (board/get-non-empty-cells board)]
    (doall (map #(draw-cell % (:size (:cell game))) non-empty-cells))))

(defn draw-win
  [move {game :game}]
  (let [c1 (first move)
        c2 (last move)
        size (:size (:cell game))
        translate (fn [x] (+ (/ size 2) (* size x)))
        c1-x (translate (nth c1 0))
        c1-y (translate (nth c1 1))
        c2-x (translate (nth c2 0))
        c2-y (translate (nth c2 1))]
    (q/stroke 0 0 0)
    (q/stroke-weight 5)
    (draw-line [c1-x c1-y c2-x c2-y])))
