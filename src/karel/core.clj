(ns karel.core
  (:require
    [quil.core :as q]
    [quil.middleware :as m]))

(def walk-dir
  {0 {:x inc}
   90 {:y inc}
   180 {:x dec}
   270 {:y dec}})

(defn entity [x y]
  {:x x :y y :z 0 :angle 0})

(defn pos [entity]
  [(:x entity) (:y entity)])

(defn get-karel [state]
  (first (:karel state)))

(defn on-chip? [state]
  (let [karel (get-karel state)
        k-pos (pos karel)]
    (first (filter #(= k-pos (pos %)) (:chips state)))))

(defn turn [state]
  (let [karel (get-karel state)
        angle (:angle karel)
        next-angle (+ angle 90)
        next-karel
          [(assoc karel :angle (mod next-angle 360))]]
    (assoc state :karel next-karel)))

(defn step "state is a vector of entities"
  [state]
  (let [karel (get-karel state)
        angle (:angle karel)
        dir-fn (get walk-dir angle)
        dir (first (keys dir-fn))
        walk-fn (first (vals dir-fn))
        next-karel (update karel dir walk-fn)]
    (if (seq (filter
               (fn [w] (= (pos next-karel) (pos w)))
               (:walls state)))
      state
      (assoc state :karel [next-karel]))))

(defn make-vertical-line [x y-ini y-end]
  (for [y (range y-ini (inc y-end))]
    (entity x y)))

(defn make-horizontal-line [y x-ini x-end]
  (for [x (range x-ini (inc x-end))]
    (entity x y)))

(defn grab [state]
  (if-let [chip (on-chip? state)]
    (let [chips (:chips state)
          new-chip (update chip :z inc)]
      (assoc state :chips
          (conj
            (remove (fn [c] (= chip c)) chips)
            new-chip)))
    state))

(defn ->pos [entities]
  (map (fn [e] (pos e)) entities))

(def s1
  {:karel [(entity 1 1)]
   :chips [(entity 2 1)]
   :goals [(entity 6 2)]
   :walls
     (concat
       (make-horizontal-line 0 0 7)
       (make-horizontal-line 3 0 7)
       (make-vertical-line 0 0 3)
       (make-vertical-line 7 0 3))})

(comment
  (grab {:karel [(entity 1 1)]
         :chips [(entity 1 1) (entity 2 1)]})
  (let [state {:karel [(entity 1 0)]
               :chips [(entity 2 0) (entity 3 0) (entity 1 0)]
               :walls [(entity 2 0) (entity 3 0)]}]
    (on-chip? state))
  (let [state {:karel [(entity 1 0)]
               :chips [(entity 2 0) (entity 3 0)]}]
    (on-chip? state))
  (step {:karel [(entity 0 0)]})
  (turn (turn {:karel [(entity 1 0)]
               :chips [(entity 0 0)]}))
  (turn
    (step s1)))

(def L 40)
(def W 800)
(def H 600)

(defn point->quil [[x y] length]
  [(* x length)
   (* y length)])

(defn points->quil [points length]
  (map (fn [p] (point->quil p length)) points))

(defn setup "returns the initial state" []
  (q/stroke 0xffa8d0db)
  (q/stroke 255 255 0)
  (q/stroke-weight 1)
  {:scenario s1
   :karel (q/load-image "resources/head.png")
   :walls (q/load-image "resources/box32.png")
   :chips (q/load-image "resources/circle32.png")
   :goals (q/load-image "resources/square.png")})

(defn the-key-handler [{:keys [scenario] :as state} k]
  (assoc state :scenario
      (case (:key-code k)
        71 (grab scenario) ; G 
        74 (turn scenario) ; J 
        75 (step scenario) ; K 
        scenario)))

(defn update-state [state]
  state)

(defn draw-element "{:type [x y]}"
  [[x y] img]
  (q/image img x y))

(defn draw-state [state]
  (q/clear)
  (q/background 255 255 255)
  (doseq [kind [:walls :chips :karel :goals]]
    (let [scenario (-> state :scenario)
          points (->pos (-> scenario
                            kind))]
      (doseq [[x y] (points->quil points L)]
        (draw-element [x y] (kind state))))))

(q/defsketch label-maker
  :title "karel"
  :size [W H]
  ; setup function called only once, during sketch nitialization.
  :setup setup
  ; update is called on each iteration before draw.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :key-pressed the-key-handler
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode m/pause-on-error])
