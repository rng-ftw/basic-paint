
(ns paint.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.math.numeric-tower :as math]))

(defn run [] (use 'paint.core :reload-all))
(defn get-mouse-loc []
  [(q/mouse-x) (q/mouse-y)])
(defn line [state]
  (let [m-loc (:mouse-loc-data state)
        b-rel (:button-release state)
        color (:draw-color state)
        draw-width (:draw-width state)
        [x y] (get-mouse-loc)
        {{:keys [min-x max-x min-y max-y]} :bounding-box} state]
    (cond
      (<= 2 (count (peek m-loc))) (assoc state :mouse-loc-data (conj (:mouse-loc-data state) []))
      (and  b-rel (not (and  (<= min-x  x max-x) (<= min-y y max-y)))
            (not= [x y] (take-last 2 (peek (peek m-loc)))));checks for dupes
      (assoc state :mouse-loc-data (conj (pop m-loc) (conj (peek m-loc) [color draw-width x y])) :button-release nil)
      :else state)))

(defn drawing [state]
  (let [mouse-loc-data (:mouse-loc-data state)
        draw-color (:draw-color state)
        draw-width (:draw-width state)
        [x y] (get-mouse-loc)
        button-pressed (:button-pressed state)
        {{:keys [min-x max-x min-y max-y]} :bounding-box} state]
    (cond
      (and (not button-pressed) (seq (peek mouse-loc-data))) (assoc state :mouse-loc-data (conj mouse-loc-data []));adds [] to the end to seprate lines drawn
      (and button-pressed
           (not (and  (<= min-x x max-x) (<= min-y y max-y))); stops adding when clicking on buttons
           (not= [x y]  (take-last 2 (peek (peek mouse-loc-data))))) ; removes dupes
      (assoc state :mouse-loc-data (conj (pop mouse-loc-data) (conj (peek mouse-loc-data) [draw-color draw-width x y])))
      :else state)))

(defn save [state]
  (when-not (state :file-saved) (spit "test.txt" (state :mouse-loc-data)))
  true)

(defn loading [state]

  (if (.exists (io/file "test.txt"))
    (let [color (:color (state :button-selected))]
      (if (not (state :file-loaded))
        (assoc state :mouse-loc-data (edn/read-string (slurp "test.txt"))
               :file-loaded true :button-selected {:func nil :color color}) state))
    state))

(def buttons ; add buttons here
  [{:name "line" :color false} {:name "draw" :color false}
   {:name "save" :color false} {:name "load" :color false}
   {:name "undo" :color false}

   {:name "red"  :color -65536}
   {:name "green" :color -16711936} {:name "blue" :color -16776961}
   {:name "black" :color -16777216} {:name "white" :color -1}])

(def height 40) ;button height
(def width 95) ;button width

(defn setup []
  (q/frame-rate 60)
  ; setup function returns initial state.
  (let [button-loc-y (mapv #(into [] [% (+ % (- height 1))]) (range 0 (* (count buttons) height) height)) ; avoids overlapping buttons
        button-loc-x [0 width]
        bounding-box {:min-x (first button-loc-x) :max-x (last button-loc-x) :min-y (first (first button-loc-y)) :max-y (last (last button-loc-y))}]
    {:file-saved false
     :file-loaded false
     :undo false

     :button-names (mapv #(:name %) buttons) ;["line" "draw" "save" "load" "undo" "red" "green" "blue" "black" "white"]
     :color-names {"red" -65536 "green" -16711936 "blue" -16776961 "black" -16777216 "white" -1}
     :button-loc  (mapv #(vector 0 % width height) (range  0 (* (count buttons) height) height)) ;format is [x y width height]
     :button-loc-y (mapv #(into [] [% (+ % (- height 1))]) (range 0 (* (count buttons) height) height)) ; avoids overlapping buttons
     :button-loc-x [0 width]
     :bounding-box bounding-box

     :button-selected {:func nil :color "black"}
     :button-release nil
     :mouse-loc-data  [[]]; last one needs to be empty [[]] format is [[color width x y]]
     :current-mouse-loc nil
     :button-pressed false
     :draw-color 0
     :draw-width 15}))

(defn which-button [x y button-state button-loc-y button-names bounding-box]
  (let [{:keys [min-x max-x min-y max-y]} bounding-box]
    (if (and  (<= min-x  x max-x) (<= min-y y max-y))
      (let [f-button
            (button-names (first (keep-indexed
                                  (fn [index item]
                                    (let [[a b] item]
                                      (when (<= a y b)
                                        index))) button-loc-y)))
            c-button "red"]
        (assoc button-state :color c-button :func f-button))
      button-state)))
(defn undo [state]
  (let [color (:color (state :button-selected))
        m-loc-data (:mouse-loc-data state)]

    (if (not-empty (pop m-loc-data))
      (assoc state :button-selected {:func nil :color color}
             :mouse-loc-data (conj (pop (pop m-loc-data)) []))
      (assoc state :button-selected {:func nil :color color}))))

(defn update-state [state]
  (let [func  (:func  (:button-selected state))]
    (cond-> state
      (= func  "undo") (undo)
      (not= func "undo") (assoc :undo false)
	;clears screen 
      (and (q/key-pressed?) (= (q/key-as-keyword) :c)) (assoc :mouse-loc-data [[]])
    ;draw
      (= func "draw") (drawing)
    ;line
      (= func "line") (line)
    ;load
      (= func "load") (loading)
      (not= func "load") (assoc :file-loaded false)
    ;save
      (and (= func "save") (not (:file-saved state))) (assoc :file-saved (save state))
      (not= func "save") (assoc :file-saved false))))

(defn draw-button [x y width height b-name b-pressed]
  (q/stroke 0)
  (q/stroke-weight 1)
  (if b-pressed (q/fill 0 255 200) (q/fill 0 0 200))
  (q/rect x y width height 1  1 10 1)
  (q/fill 40 90 200)
  (q/rect (+ x 4) (+ y 4) (- width 8) (- height  8) 1  1 10 1)
  (q/fill 255 255 255)
  (q/text-align :left :top)
  (q/text b-name  (+ x 10)  y))

(defn draw-screen [state]
  (q/background 200)
  (q/text-size 30)
  (q/stroke-weight 1)
  (dorun  ;function buttons
   (map (fn [[x y width height] button-names] (draw-button  x y width height button-names
    ; this allows for 2 buttons to be highlighted a func and color button
                                                            (or (= (:func (:button-selected state)) button-names)
                                                                (= (:color (:button-selected state)) button-names))))
        (:button-loc state) (:button-names state)))

  (q/text-align :center :center)
  (when (q/key-pressed?) (q/text (str "key as keyword:" (q/key-as-keyword)) (q/mouse-x) (+ (q/mouse-y) 30)))
  (when (q/mouse-pressed?) (q/text (str (q/mouse-x) " " (q/mouse-y)) (q/mouse-x) (+ (q/mouse-y) 10)))

  (when (seq (:mouse-loc-data state)); needs to enventual be removed
    (dorun (map
            #(cond
               (= 0 (count %)) nil
               (= 1 (count %)) ((fn [[[color width x y]]] (q/stroke color) (q/fill color) (q/stroke-weight 0) (q/ellipse x y  width width)) %) ;individual pixels are really small :p
               (< 1 (count %)) (doall (map (fn [[color width x1 y1 - -  x2 y2]] (q/stroke color) (q/stroke-weight width) (q/line x1 y1 x2 y2)) (partition 8 4 (flatten %))))) (:mouse-loc-data state))))

  (q/stroke (state :draw-color))
  (q/fill (state :draw-color))
  (q/stroke-weight 0)
  (q/ellipse (q/mouse-x) (q/mouse-y)  (:draw-width state) (:draw-width state))

  state)

(defn click [state m-state]
  (let [b-clicked (which-button (m-state :x) (m-state :y) (:button-selected state) (:button-loc-y state) (:button-names state) (:bounding-box state))
        c-clicked (:color b-clicked)]
    (assoc state :button-pressed true :button-selected b-clicked :draw-color ((:color-names state) c-clicked))))

(defn wheel [state w-state]
  (let [num (math/abs (+ (:draw-width state) w-state))] ;make sure :draw-width doesnt go negative
    (assoc state :draw-width num)))

(defn m-release [state r-state]
  (let [x (:x r-state)
        y (:y r-state)
        min-x (:min-x (:bounding-box state))
        max-x (:max-x (:bounding-box state))
        min-y (:min-y (:bounding-box state))
        max-y (:max-y (:bounding-box state))]
    (if (and (not (and (<= min-x  x max-x) (<= min-y y max-y)))
             (= (:func (:button-selected state)) "line"))
      (assoc state :button-release r-state :button-pressed false)
      (assoc state :button-pressed false))))

(q/defsketch paint
  :title "draw: hit C to clear screen : use scroll wheel to resize paintbrush"
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-screen
  :mouse-pressed click
  :mouse-released m-release
  :mouse-wheel wheel
  :features [:resizable]
  :middleware [m/fun-mode])
