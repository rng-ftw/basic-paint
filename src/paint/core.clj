
(ns paint.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.math.numeric-tower :as math]))

(defn run [] (use 'paint.core :reload-all))

(defn line [state]
  (let [m-state (state :mouse-state)
        m-loc (:mouse-loc state)
        b-rel (state :button-release)
        x     (:x (state :button-release))
        y     (:y (state :button-release))]
    (cond
      (and (not m-state) (<= 2 (count (peek m-loc)))) (assoc state :mouse-loc (conj (state :mouse-loc) []))
      (and  b-rel (not= [x y] (take-last 2 (peek (peek m-loc)))));checks for dupes
      (assoc state :mouse-loc (conj (pop m-loc) (conj (peek m-loc) [(:draw-color state) (:draw-thickness state) x y])) :button-release nil)
      :else state)))

(defn drawing [state]
  (cond
    (and (not (state :mouse-state)) (seq (peek (state :mouse-loc)))) (conj (state :mouse-loc) []);adds [] to the end to seprate lines drawn
    (and (state :mouse-state)
         (not= [(q/mouse-x) (q/mouse-y)]  (take-last 2 (peek (peek (:mouse-loc state)))))) ; removes dupes
    (conj (pop (state :mouse-loc)) (conj (peek (state :mouse-loc)) [(:draw-color state) (:draw-thickness state) (q/mouse-x) (q/mouse-y)]))
    :else (state :mouse-loc)))

(defn save [state]
  (when-not (state :file-saved) (spit "test.txt" (state :mouse-loc)))
  true)

(defn loading [state]
  (if (.exists (io/file "test.txt"))
    (let [color (:color (state :button-state))]
      (if (not (state :file-loaded))
        (assoc state :mouse-loc (edn/read-string (slurp "test.txt"))
               :file-loaded true :button-state {:func nil :color color}) state))
    state))

(defn setup []
  (q/frame-rate 60)
  ; setup function returns initial state. 
  {:file-saved false
   :file-loaded false
   :button-loc  (mapv #(vector 0 % 100 50) [100 150 200 250 300 350 400 450 500]) ;format is [x y with height]
   :color-names {"red" -65536 "green" -16711936 "blue" -16776961 "black" -16777216 "white" -1}
   :button-names ["line" "draw" "save" "load" "red" "green" "blue" "black" "white"]
   :button-state {:func nil :color "black"}
   :button-release nil
   :mouse-state nil
   :mouse-loc  [[]]; last one needs to be empty [[]] format is [[color width x y]]
   :draw-color 0
   :draw-thickness 15})

(defn which-button [x y button-state]
  (if (and (>= 100 x) (<= 0 x) (<= 100 y) (>= 550 y))
    (let [f-button
          (cond ; todo fix this so I dont have to add stuff to multiple places in order to add another button
            (< 100 y 150) "line"
            (< 150 y 200) "draw"
            (< 200 y 250) "save"
            (< 250 y 300) "load"
            :else (button-state :func))
          c-button
          (cond
            (< 300 y 350) "red"
            (< 350 y 400) "green"
            (< 400 y 450) "blue"
            (< 450 y 500) "black"
            (< 500 y 550) "white"
            :else (button-state :color))]
      (assoc button-state :color c-button :func f-button))
    button-state))

(defn update-state [state]
  (cond-> state
  ;stop initial button click from being added to drawing
    (and (q/mouse-pressed?) (not (and (>= 100 (q/mouse-x)) (<= 0 (q/mouse-x)) (<= 100 (q/mouse-y)) (>= 550 (q/mouse-y))))) (assoc :mouse-state true)
    (not (q/mouse-pressed?)) (assoc :mouse-state false)
    ;switch to color chosen
    (and (:color (state :button-state)) (not= (:color (state :button-state)) (state :draw-color)))
    (assoc :draw-color ((state :color-names) (:color (state :button-state))))
    ;draw
    (= (:func (state :button-state)) "draw") (assoc :mouse-loc (drawing state))
    ;line
    (= (:func (state :button-state)) "line") (line)
     ;clears screen 
    (and (q/key-pressed?) (= (q/key-as-keyword) :c)) (assoc :mouse-loc [[]]) ;needs to be at the below draw and line to work ???  
    ;load
    (= (:func (state :button-state)) "load") (loading)
    (not= (:func (state :button-state)) "load") (assoc :file-loaded false)
    ;save
    (and (= (:func (state :button-state)) "save") (not (state :file-saved))) (assoc :file-saved (save state))
    (not= (:func (state :button-state)) "save") (assoc :file-saved false)))

(defn draw-button [x y width height b-name b-pressed]
  (q/stroke 0)
  (q/stroke-weight 1)
  (if b-pressed (q/fill 0 255 200) (q/fill 0 0 200))
  (q/rect x y width height 1  1 10 1)
  (q/fill 40 90 200)
  (q/rect (+ x 4) (+ y 4) (- width 8) (- height  8) 1  1 10 1)
  (q/fill 255 255 255)
  (q/text-align :center)
  (q/text b-name (+ x 45) (+ y 35)))

(defn draw-screen [state]
  (q/background 200)
  (q/text-size 30)
  (q/stroke-weight 1)

  (doall  ;function buttons
   (map (fn [[x y width height] button-names] (draw-button  x y width height button-names
    ; this allows for 2 buttons to be highlighted a func and color button
                                                            (or (= (:func (state :button-state)) button-names)
                                                                (= (:color (state :button-state)) button-names))))
        (state :button-loc) (state :button-names)))

  (q/text-align :center :center)
  (when (q/key-pressed?) (q/text (str "key as keyword:" (q/key-as-keyword)) (q/mouse-x) (+ (q/mouse-y) 30)))
  (when (q/mouse-pressed?) (q/text (str (q/mouse-x) " " (q/mouse-y)) (q/mouse-x) (+ (q/mouse-y) 10)))

  (when (seq (state :mouse-loc)); needs to enventual be removed
    (doall (map
            #(cond
               (= 0 (count %)) nil
               (= 1 (count %)) ((fn [[[color width x y]]] (q/stroke color) (q/fill color) (q/stroke-weight 0) (q/ellipse x y  width width)) %) ;individual pixels are really small :p
               (< 1 (count %)) (doall (map (fn [[color width x1 y1 - -  x2 y2]] (q/stroke color) (q/stroke-weight width) (q/line x1 y1 x2 y2)) (partition 8 4 (flatten %))))) (state :mouse-loc))))
  state)

(defn click [state m-state]
  (assoc state :button-state (which-button (m-state :x) (m-state :y) (state :button-state))))

(defn wheel [state w-state]
  (let [num (math/abs (+ (state :draw-thickness) w-state))] ;make sure :draw-thickness doesnt go negative
    (println "wheel" w-state num)
    (assoc state :draw-thickness num)))

(defn m-release [state r-state]
  (println "button released" r-state)
  (let [x (:x r-state) y (:y r-state)]
    (if (not (and (>= 100 x) (<= 0 x) (<= 100 y) (>= 550 y)))
      (do (println "changed :button-release")
          (assoc state :button-release r-state)) state)))

(q/defsketch paint
  :title "draw: hit C to clear screen"
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-screen
  :mouse-clicked click
  :mouse-released m-release
  :mouse-wheel wheel
  :features [:resizable]
  :middleware [m/fun-mode])
