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
(def game-state (atom :playing))
(def removed-stones (atom #{}))

; Board specific functions
(defn square-pos-to-screen [[i j]]
  (let [size (dec (get-in @game [:board :size]))
        x (+ board-offset-x (* board-width (/ i size)))
        y (+ board-offset-y (* board-height (/ j size)))]
    [x y]))

(defn hexa-pos-to-screen [[i j]]
  (let [size (get-in @game [:board :size])
        full-size (inc (* 2 (dec size)))
        px-size (* board-width (/ 1 size 1.25))
        half-size (quot full-size 2)
        dj (if (<= i half-size) (- half-size i) (- i half-size))
        board-offset-x (- board-offset-x (/ px-size 2))
        board-offset-y (- board-offset-y (/ px-size 2))
        x (+ board-offset-x (* (Math/sqrt (/ 3 4)) (* i px-size)))
        y (+ board-offset-y (* (/ px-size 2) dj) (* j px-size))]
    [x y]))

(defn make-cell-positions [f]
  (let [cs (g/get-cells @game)]
    (zipmap cs (map f cs))))

; Sketch functions
(defn setup []
  (q/frame-rate 30)
  (q/smooth 2))

(defn draw-lines []
  (q/stroke 0)
  (doseq [[pos [x1 y1]] @cell-positions]
    (let [neighbors (g/get-neighbors-at @game pos)]
      (doseq [n neighbors]
        (let [[x2 y2] (get @cell-positions n)]
          (q/line x1 y1 x2 y2))))))

(defn set-draw-color [turn & {:keys [transparency] :or {transparency 255}}]
  (cond (= turn :black) (do (q/stroke 0 0 0 transparency) (q/fill 0 0 0 transparency))
        (= turn :white) (do (q/stroke 0 0 0 transparency) (q/fill 255 255 255 transparency))))

(defn draw-stones []
  (doseq [[pos [x y]] @cell-positions]
    (let [color (g/get-at @game pos)]
      (when (and (not= color nil))
        (if (and (= :remove-dead @game-state) (contains? @removed-stones pos))
          (set-draw-color color :transparency 127)
          (set-draw-color color))
        (q/ellipse x y @stone-size @stone-size)))))

(defn distance [x1 y1 x2 y2]
  (Math/sqrt (+ (Math/pow (- x2 x1) 2) (Math/pow (- y2 y1) 2))))

(defn find-closest-key [positions mouse-pos]
  (let [[x1 y1] mouse-pos
        distances (into {} (for [[key [x2 y2]] positions]
                             [key (distance x1 y1 x2 y2)]))]
    (first (apply min-key second distances))))

(defn draw-mouse-over []
  (when (= :playing @game-state)
    (let [x (q/mouse-x)
          y (q/mouse-y)]
      (let [pos (find-closest-key @cell-positions [x y])
            [sx sy] (get @cell-positions pos)
            color (g/get-turn @game)
            already-drawn (g/get-at @game pos)]
        (when (= nil already-drawn)
          (set-draw-color color :transparency 127)
          (q/ellipse sx sy @stone-size @stone-size))))))

(defn draw-text []
  (q/stroke 0)
  (q/fill 0)
  (q/text-size 25)
  (cond
    (= :playing @game-state) (q/text "Space to pass" 5 25)
    (= :remove-dead @game-state) (q/text "Click to remove stone, space to finish" 5 25)
    (= :score @game-state) (q/text "Game finished" 5 25)))

(defn draw-score []
  (q/stroke 0)
  (q/fill 0)
  (q/text-size 25)
  (let [score (:score @game)]
    (q/text (str "Black: " (:black score) "\nWhite: " (:white score)) 5 (- height 50))))

(defn draw []
  (q/background 200)
  (draw-lines)
  (draw-stones)
  (draw-mouse-over)
  (draw-score)
  (draw-text))

(defn mouse-clicked []
  (let [x (q/mouse-x)
        y (q/mouse-y)
        pos (find-closest-key @cell-positions [x y])]
    (cond
      (= :playing @game-state) (swap! game g/play-turn pos)
      (= :remove-dead @game-state) (swap! removed-stones (if (contains? @removed-stones pos) disj conj) pos)
      (= :score @game-state) (do))))

(defn key-pressed []
  (let [raw-key (q/raw-key)]
    (when (= raw-key \space)
      (cond
        (= :playing @game-state) (do
                                   (swap! game g/pass)
                                   (when (g/finished? @game)
                                     (reset! game-state :remove-dead)))
        (= :remove-dead @game-state) (do
                                       (reset! game-state :score)
                                       (doseq [s @removed-stones]
                                         (swap! game g/remove-stone s))
                                       (swap! game g/score))
        (= :score @game-state) (do)))))

; Main
(defn -main [& args]
  (let [mode (or (first args) "square")
        size (second args)
        size (if (not= size nil) (Integer/parseInt size) 9)]
    (cond
      (= "square" mode) (do
                          (reset! game (g/make-square-game size))
                          (reset! cell-positions (make-cell-positions square-pos-to-screen))
                          (reset! stone-size (/ board-width size)))
      (= "hexa" mode) (do
                        (reset! game (g/make-hexa-game size))
                        (reset! cell-positions (make-cell-positions hexa-pos-to-screen))
                        (reset! stone-size (/ board-width (inc (* 2 (dec size))))))))
  (q/defsketch hexago
               :title "Hexago"
               :size [width height]
               :settings #(q/smooth 3)
               :setup setup
               :draw draw
               :mouse-clicked mouse-clicked
               :key-pressed key-pressed))
