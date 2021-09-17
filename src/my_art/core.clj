
(ns paint.core
  (:require [quil.core :as q :include-macros true ]
            [quil.middleware :as m]
			[clojure.java.io :as io]))
(defn output[string state] (println string state)state) ;used for testing output of function 
(defn run [] (use 'paint.core :reload-all))
;(def toggle_state (atom {:file_loaded false}))
(defn line[state]
(cond
	(and (not (q/mouse-pressed?))(<= 2(count(peek (:mouse_loc state)))))(conj (state :mouse_loc) []) 
	(and (state :mouse_state)(not (q/mouse-pressed?))
		 (not= [(q/mouse-x) (q/mouse-y)](take-last 2(peek(peek (:mouse_loc state))))));checks for dupes
				(conj (pop (state :mouse_loc)) (conj (peek (state :mouse_loc)) [(:draw_color state)(:draw_thickness state) (q/mouse-x) (q/mouse-y)]))
	:else (state :mouse_loc)
))
(defn drawing[state]
;(println  (:mouse_loc state))
   (cond
     (and (not (state :mouse_state))(seq (peek (state :mouse_loc)))) (conj (state :mouse_loc) []);adds [] to the end to seprate lines drawn
     (and (state :mouse_state) 
          (not= [(q/mouse-x) (q/mouse-y)]  (take-last 2(peek(peek (:mouse_loc state)))))) ; removes dupes
	 (conj (pop (state :mouse_loc)) (conj (peek (state :mouse_loc)) [(:draw_color state) (:draw_thickness state) (q/mouse-x) (q/mouse-y)]))
     :else (state :mouse_loc))
  )
(defn save[state]
	(when (not(state :file_saved)) (spit "test.txt" (state :mouse_loc)))
	;(assoc state :file_saved true) ; Makes sure the files is saved once and not (q/frame-rate) times a second
	true
	)
(defn loading[state] 
	(if (.exists(io/file "test.txt"))
	(if (not (state :file_loaded))(assoc state :mouse_loc(read-string (slurp "test.txt")):file_loaded true :button_state nil)state)
	state))
(defn setup []
  (q/frame-rate 60)
  (q/color-mode :rgb)
  ; setup function returns initial state. 
  {
  :file_saved false
  :file_loaded false
  :button_loc  (mapv #( into [] [0  %1 (%2 0)(%2 1)]) [100 150 200 250](repeat [100 50]))
  :button_names ["line" "draw" "save" "load"]
  :button_state nil
  :mouse_state nil
  :mouse_loc  [[]]; last one needs to be empty [[]] format is [[color width x y]]
  :draw_color 255
  :draw_thickness 15
   })
(defn which-button[x y button_state ] 
  (if (and(>= 100 x)(<= 0 x)(<= 100 y)(>= 300 y))
    (cond ; todo fix this so I dont have to add stuff to multiple places in order to add another button
       (and (< 100 y)(> 150 y))"line"
       (and (< 150 y)(> 200 y))"draw"
       (and (< 200 y)(> 250 y))"save"
       (and (< 250 y)(> 300 y))"load"
	   :else nil ;just in case?
       )
    button_state
    ))
(defn update-state [state]
 (cond-> state
	;draw
	  (= (state :button_state) "draw") (assoc :mouse_loc (drawing state))
	;line
	  (= (state :button_state) "line") (assoc :mouse_loc (line state))
	 ;clears screen 
	  (and (q/key-pressed?) (= (q/key-as-keyword) :c)) (assoc :mouse_loc [[]] ) ;needs to be at the below draw and line to work ???  
	;load
	  (= (state :button_state) "load") (loading)
	  (not= (state :button_state) "load") (assoc :file_loaded false)
	;save
	  (and (= (state :button_state) "save")(not(state :file_saved))) (assoc :file_saved (save state))
	  (not= (state :button_state) "save") (assoc :file_saved false)
	;stop button click from being added to drawing
	  (and(q/mouse-pressed?)(not(and(>= 100 (q/mouse-x))(<= 0 (q/mouse-x))(<= 100 (q/mouse-y))(>= 300 (q/mouse-y)))))(assoc :mouse_state true)
	  (not(q/mouse-pressed?)) (assoc :mouse_state false)
	  ) 
	 ; { 
	 ; :file_saved  (if (and (= (state :button_state) "save")(not(state :file_saved)))(save state)(state :file_saved))
	 ; :button_loc (do(:file_saved true )(state  :button_loc))
	 ; :button_names (state :button_names)
	 ; :mouse_state (if (and(q/mouse-pressed?)(not(and(>= 100 (q/mouse-x))(<= 0 (q/mouse-x))(<= 100 (q/mouse-y))(>= 300 (q/mouse-y))))) true false); not really keen on this one, this stops drawing  from/to on screen buttons which doesnt really work
	 ; :button_state (state :button_state);
	 ; :draw_color (state :draw_color)
	 ; :draw_thickness (state :draw_thickness)
	 ; :mouse_loc 
                ; (cond
				 ; (and (q/key-pressed?) (= (q/key-as-keyword) :c))  [[]] 
                ; ;draw
                  ; (= (state :button_state) "draw") (drawing state)
                ; ;line
                  ; (= (state :button_state) "line") (line state)
				; ;load
				  ; (= (state :button_state) "load") (if (not(@toggle_state :file_loaded)) ( load)(state :mouse_loc))
				 
                 ; :else (state :mouse_loc)
                  ; ) } 
)				 			 
(defn draw-button [x y width height b-name b-pressed]
  (q/stroke 0)
  (q/stroke-weight 1)
  (if b-pressed(q/fill 0 255 200)(q/fill 0 0 200))
  (q/rect x y width height 1  1 10 1)
  (q/fill 40 90 200)
  (q/rect (+ x 4) (+ y 4)(- width 8) (- height  8) 1  1 10 1)
  (q/fill 255 255 255)
  (q/text-align :center)
  (q/text b-name (+ x 45) (+ y 35))
  )
(defn draw-screen [state]
  (q/background 200)
  (q/text-size 30)
  (q/stroke 0)
  (q/stroke-weight 10)
  (q/line 0 0 (q/width)0);draw line at top 
  
  (dorun (map #(draw-button (%1 0)(% 1)(% 2)  (% 3) %2 (=(state :button_state)%2 )) (state :button_loc) (state  :button_names)) )
 
  (q/fill 255 0 0)
  (q/text-align :center :center)
  (when (q/key-pressed?) (q/text (str "key as keyword:"(q/key-as-keyword)) (q/mouse-x) (+(q/mouse-y)30))   )
  (when (q/mouse-pressed?) (q/text (str (q/mouse-x)" " (q/mouse-y)) (q/mouse-x) (+ (q/mouse-y) 10)  )) 

(when (seq (state :mouse_loc)); needs to enventual be removed
  (doall (map
  #(cond
	 (= 0 (count %)) nil
	 (= 1 (count %)) ((fn [[[c1 w1 x y]]](q/stroke c1 0 255)(q/stroke-weight w1) (q/ellipse x y  2 2 )) %) ;individual pixels are really small :p
	 (< 1 (count %)) (doall (map (fn[[c1 w1 a b _ _  c d]](q/stroke c1 0 255)(q/stroke-weight w1)(q/line a b c d))(partition 8 4(flatten %))))
	 )(state :mouse_loc)
 )))
state)
(defn click [state m_state]
(assoc state :button_state (which-button (m_state :x)(m_state :y)(state :button_state)) ) )
(q/defsketch paint
  :title "draw: hit C to clear screen"
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-screen
  :mouse-clicked click
  :features [:resizable]
  :middleware [m/fun-mode])
