(ns karel.core
  (:require
    [clojure.string :as str]))

(def walk-dir
  {0 {:x inc}
   90 {:y inc}
   180 {:x dec}
   270 {:y dec}})

(defn entity [x y t]
  {:x x :y y :type t :angle 0})

(defn karel [x y]
  (entity x y :karel))

(defn wall [x y]
  (entity x y :wall))

(defn chip [x y]
  (entity x y :chip))

(defn get-entities [state kind]
  (filter
    #(= (get-in % [:type]) kind)
    state))

(defn get-karel [state]
  (-> state
      (get-entities :karel)
      first))

(defn pos [entity]
  [(:x entity) (:y entity)])

(defn on-chip? [state]
  (let [karel (get-karel state)
        k-pos (pos karel)
        cs-pos (map pos (get-entities state :chip))]
    (seq (filter #(= k-pos %) cs-pos))))

(defn walk [{:keys [angle] :as entity}]
  (let [dir-fn (get walk-dir angle)
        dir (first (keys dir-fn))
        walk-fn (first (vals dir-fn))]
    (update entity dir walk-fn)))

(defn turn [{:keys [angle] :as entity}]
  (let [next-angle (+ angle 90)]
    (assoc entity :angle (mod next-angle 360))))

(defn step "state is a vector of entities"
  [state]
  (if-let [karel (get-karel state)]
    (walk karel)
    state))

(defn make-vertical-line [kind x y-ini y-end]
  (for [y (range y-ini y-end)]
    (entity x y kind)))

(defn make-horizontal-line [kind y x-ini x-end]
  (for [x (range x-ini x-end)]
    (entity x y kind)))

(def s1
  (concat
    (make-horizontal-line :wall 0 0 5)
    (make-horizontal-line :wall 5 0 5)
    (make-vertical-line :wall 0 0 5)
    (make-vertical-line :wall 5 0 5)))

(comment
  (let [state [(karel 1 0)
               (chip 2 0) (chip 3 0) (chip 1 0)
               (wall 2 0) (wall 3 0)]]
    (on-chip? state))
  (let [state [(karel 1 0) (wall 2 0) (wall 3 0)]]
    (on-chip? state))
  (get-karel [(karel 1 0) (wall 1 0) (wall 1 0)])
  (get-entities [(karel 1 0) (wall 2 0) (wall 3 0)] :wall)
  (step [(wall 0 0)])
  (chip 0 0)
  (turn (turn (karel 1 0)))
  (turn
    (step [(karel 1 0) (wall 1 1)])))
