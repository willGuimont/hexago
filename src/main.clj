(ns main
  (:require [game :as g]
            [quil.core :as q]))

; Variables
(def width 800)
(def height 800)
(def board-width 500)
(def board-height 500)
(def board-offset-x (/ (- width board-width) 2))
(def board-offset-y (/ (- height board-height) 2))
(def stone-size (atom 10))
(def game (atom nil))
(def cell-positions (atom {}))

; Board specific functions
(defn pos-to-screen [[i j]]
  (let [size (dec (get-in @game [:board :size]))
        x (+ board-offset-x (* board-width (/ i size)))
        y (+ board-offset-y (* board-height (/ j size)))]
    [x y]))

(defn square-cell-positions []
  (let [cs (g/get-cells @game)]
    (zipmap cs (map pos-to-screen cs))))

; Sketch functions
(defn setup []
  (q/frame-rate 30))

(defn draw-lines []
  (q/color 0)
  (doseq [[pos [x1 y1]] @cell-positions]
    (let [neighbors (g/get-neighbors-at @game pos)]
      (doseq [n neighbors]
        (let [[x2 y2] (get @cell-positions n)]
          (q/line x1 y1 x2 y2))))))

(defn set-draw-color [turn & {:keys [transparency] :or {transparency 255}}]
  (cond (= turn :black) (do (q/color 255 255 255 transparency) (q/fill 0 0 0 transparency))
        (= turn :white) (do (q/color 0 0 0 transparency) (q/fill 255 255 255 transparency))))

(defn draw-stones []
  (doseq [[pos [x y]] @cell-positions]
    (let [color (g/get-at @game pos)]
      (when (not= color nil)
        (set-draw-color color)
        (q/ellipse x y @stone-size @stone-size)))))

(defn distance [x1 y1 x2 y2]
  (Math/sqrt (+ (Math/pow (- x2 x1) 2) (Math/pow (- y2 y1) 2))))

(defn find-closest-key [positions mouse-pos]
  (let [[x1 y1] mouse-pos
        distances (into {} (for [[key [x2 y2]] positions]
                             [key (distance x1 y1 x2 y2)]))]
    (first (apply min-key second distances))))

(defn draw-mouse-over []
  (let [x (q/mouse-x)
        y (q/mouse-y)]
    (let [pos (find-closest-key @cell-positions [x y])
          [sx sy] (get @cell-positions pos)
          color (g/get-turn @game)
          already-drawn (g/get-at @game pos)]
      (when (= nil already-drawn)
        (set-draw-color color :transparency 127)
        (q/ellipse sx sy @stone-size @stone-size)))))

(defn draw-instruction []
  (q/color 0)
  (q/fill 0)
  (q/text-size 25)
  (q/text "Space to pass" 0 25))

; TODO score when the game is finished
(defn draw []
  (q/background 200)
  (draw-lines)
  (draw-stones)
  (draw-mouse-over)
  (draw-instruction))

(defn mouse-clicked []
  (let [x (q/mouse-x)
        y (q/mouse-y)
        pos (find-closest-key @cell-positions [x y])]
    (swap! game g/play-turn pos)))

(defn key-pressed []
  (let [raw-key (q/raw-key)]
    (when (= raw-key \space)
      (swap! game g/pass))))

; Main
(defn -main [& args]
  ; TODO make a better CLI interface
  (let [mode (or (first args) "square")
        size (or (second args) 9)]
    ; TODO add hexagonal grid
    (reset! game (cond
                   (= "square" mode) (g/make-square-game size)))
    ; TODO add hexagonal grid
    (reset! cell-positions (cond
                             (= "square" mode) (square-cell-positions)))
    (reset! stone-size (/ board-width size)))
  (q/defsketch hexago
               :title "Hexago"
               :size [width height]
               :settings #(q/smooth 3)
               :setup setup
               :draw draw
               :mouse-clicked mouse-clicked
               :key-pressed key-pressed))
