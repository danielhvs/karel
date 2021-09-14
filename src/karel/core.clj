(ns karel.core
  (:require
    [quil.core :as q]
    [quil.middleware :as m]))

(def walk-dir
  {0 {:x inc}
   90 {:y inc}
   180 {:x dec}
   270 {:y dec}})

(defn entity
  ([x y z a]
   {:x x :y y :z z :angle a})
  ([x y]
   (entity x y 0 0)))

(defn pos-xy [entity]
  [(:x entity) (:y entity)])

(defn get-karel [state]
  (first (:karel state)))

(defn on-chip? [state]
  (let [karel (get-karel state)
        k-pos (pos-xy karel)]
    (first (filter #(= k-pos (pos-xy %)) (:chips state)))))

(defn turn [state]
  (let [karel (get-karel state)
        angle (:angle karel)
        next-angle (+ angle 90)
        next-karel
          [(assoc karel :angle (mod next-angle 360))]]
    (assoc state :karel next-karel)))

(defn in-front-of-wall? [state next-karel]
  (seq (filter
         (fn [w] (= (pos-xy next-karel) (pos-xy w)))
         (:walls state))))

(defn chip-handled? [c]
  (> (:z c) 0))

(defn step "state is a vector of entities"
  [state]
  (let [karel (get-karel state)
        angle (:angle karel)
        dir-fn (get walk-dir angle)
        dir (first (keys dir-fn))
        walk-fn (first (vals dir-fn))
        next-karel (update karel dir walk-fn)]
    (if (in-front-of-wall? state next-karel)
      state
      (assoc state
        :karel [next-karel]
        :chips (let [chips (:chips state)]
                 (map (fn [c]
                        (if (chip-handled? c)
                          (assoc c
                            :x (:x next-karel)
                            :y (:y next-karel))
                          c)) chips))))))

(defn make-vertical-line [x y-ini y-end]
  (for [y (range y-ini (inc y-end))]
    (entity x y)))

(defn make-horizontal-line [y x-ini x-end]
  (for [x (range x-ini (inc x-end))]
    (entity x y)))

(defn drop-chip [state]
  (if-let [chip (on-chip? state)]
    (let [chips (:chips state)
          new-chip (assoc chip :z -1)]
      (assoc state :chips
          (conj
            (remove (fn [c] (= chip c)) chips)
            new-chip)))
    state))

(defn grab [state]
  (if-let [chip (on-chip? state)]
    (let [chips (:chips state)
          new-chip (assoc chip :z 1)]
      (assoc state :chips
          (conj
            (remove (fn [c] (= chip c)) chips)
            new-chip)))
    state))

(def s1
  {:karel [(entity 1 1)]
   :chips [(entity 2 1 -1 0)]
   :goals [(entity 6 2)]
   :walls
     (concat
       (make-horizontal-line 0 0 7)
       (make-horizontal-line 3 0 7)
       (make-vertical-line 0 0 3)
       (make-vertical-line 7 0 3))})

(def s2
  {
   :karel [{:x 1 :y 2 :z 0 :angle 0}]
   :chips [{:x 1 :y 3 :z 0 :angle 0}
           {:x 3 :y 3 :z 0 :angle 0}
           {:x 5 :y 3 :z 0 :angle 0}
           {:x 7 :y 3 :z 0 :angle 0}]
   :goals [{:x 2 :y 1 :z 0 :angle 0}
           {:x 4 :y 1 :z 0 :angle 0}
           {:x 6 :y 1 :z 0 :angle 0}
           {:x 8 :y 1 :z 0 :angle 0}]
   :walls [{:x 1 :y 1 :z 0 :angle 0}
           {:x 3 :y 1 :z 0 :angle 0}
           {:x 5 :y 1 :z 0 :angle 0}
           {:x 7 :y 1 :z 0 :angle 0}
           {:x 9 :y 1 :z 0 :angle 0}
           {:x 0 :y 0 :z 0 :angle 0}
           {:x 0 :y 1 :z 0 :angle 0}
           {:x 0 :y 2 :z 0 :angle 0}
           {:x 0 :y 3 :z 0 :angle 0}
           {:x 0 :y 4 :z 0 :angle 0}
           {:x 0 :y 4 :z 0 :angle 0}
           {:x 1 :y 4 :z 0 :angle 0}
           {:x 2 :y 4 :z 0 :angle 0}
           {:x 3 :y 4 :z 0 :angle 0}
           {:x 4 :y 4 :z 0 :angle 0}
           {:x 5 :y 4 :z 0 :angle 0}
           {:x 6 :y 4 :z 0 :angle 0}
           {:x 7 :y 4 :z 0 :angle 0}
           {:x 8 :y 4 :z 0 :angle 0}
           {:x 9 :y 4 :z 0 :angle 0}
           {:x 10 :y 4 :z 0 :angle 0}
           {:x 10 :y 0 :z 0 :angle 0}
           {:x 10 :y 1 :z 0 :angle 0}
           {:x 10 :y 2 :z 0 :angle 0}
           {:x 10 :y 3 :z 0 :angle 0}
           {:x 10 :y 4 :z 0 :angle 0}
           {:x 0 :y 0 :z 0 :angle 0}
           {:x 1 :y 0 :z 0 :angle 0}
           {:x 2 :y 0 :z 0 :angle 0}
           {:x 3 :y 0 :z 0 :angle 0}
           {:x 4 :y 0 :z 0 :angle 0}
           {:x 5 :y 0 :z 0 :angle 0}
           {:x 6 :y 0 :z 0 :angle 0}
           {:x 7 :y 0 :z 0 :angle 0}
           {:x 8 :y 0 :z 0 :angle 0}
           {:x 9 :y 0 :z 0 :angle 0}
           {:x 10 :y 0 :z 0 :angle 0}]})

(comment
  (grab
    (grab {:karel [(entity 1 1)]
           :chips [(entity 1 1) (entity 2 1)]}))
  (let [state {:karel [(entity 1 0)]
               :chips [(entity 2 0) (entity 3 0) (entity 1 0)]
               :walls [(entity 2 0) (entity 3 0)]}]
    (on-chip? state))
  (let [state {:karel [(entity 1 0)]
               :chips [(entity 2 0) (entity 3 0)]}]
    (on-chip? state))
  (step {:karel [(entity 0 0)]
         :chips [(entity 0 0 -1 0)]})
  (turn (turn {:karel [(entity 1 0)]
               :chips [(entity 0 0)]}))
  (turn
    (step s1)))

(def L 32)
(def W 800)
(def H 600)

(defn point->quil [entity length]
  (assoc entity
    :x (* (:x entity) length)
    :y (* (:y entity) length)))

(defn points->quil [entities length]
  (map (fn [e] (point->quil e length)) entities))

(defn setup "returns the initial state" []
  (q/frame-rate 10)
  {:scenario s2
   :karel (q/load-image "resources/head.png")
   :walls (q/load-image "resources/box32.png")
   :chips (q/load-image "resources/circle32.png")
   :goals (q/load-image "resources/square.png")})

(def solution1
  [step grab step step step step turn step drop-chip
   turn turn step turn turn turn step step step step step
   turn turn])

(def turn-up
  [turn turn turn])

(def turn-down
  [turn])

(def down
  (into
    (into turn-down [step])
    [turn turn turn]))

(def up
  (into
    (into turn-up [step])
    [turn]))

(def grab-one
  (into down [grab]))

(def drop-one
  (flatten
    (into grab-one [step
                    up
                    up
                    drop-chip
                    down
                    step])))

(def solution2
  (flatten (repeat 4 drop-one)))

(defn fn-solution [state index solution]
  (if (< index (count solution))
    (let [event (nth solution index)]
      (assoc state :scenario
          (event (:scenario state))
        :iteration (inc index)))
    state))

(defn the-key-handler [{:keys [scenario] :as state} k]
  (case (:key-code k)
    81 (assoc state :iteration 0 :solution solution1) ; q
    87 (assoc state :iteration 0 :solution solution2) ; w
    (assoc state :scenario
        (case (:key-code k)
          49 s1 ; 1
          50 s2 ; 2
          68 (drop-chip scenario) ; d
          71 (grab scenario) ; g
          74 (turn scenario) ; j
          75 (step scenario) ; k
          scenario))))

(defn update-state [state]
  (if-let [index (:iteration state)]
    (if-let [solution (:solution state)]
      (fn-solution state index solution)
      state)
    state))

(defn draw-element "{:type [x y]}"
  [{:keys [x y z]} img]
  (if (= z 1)
    (do (q/resize img 16 0)
        (q/image img (+ 8 x) (+ 8 y)))
    (do (q/resize img 32 0)
        (q/image img x y))))

(defn sort-to-draw [scenario]
  (let [res
          (for [kind (keys scenario)]
            (map (fn [e] {:type kind
                          :x (:x e)
                          :y (:y e)
                          :z (:z e)
                          :angle (:angle e)}) (kind scenario)))]
    (->> res
         (reduce #(into %1 %2) [])
         (sort-by :z))))

(defn draw-state [state]
  (q/clear)
  (q/background 255 255 255)
  (let [scenario (-> state :scenario)
        entities (sort-to-draw scenario)]
    (doseq [e (points->quil entities L)]
      (draw-element e ((:type e) state)))))

(q/defsketch karel-sketch
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
