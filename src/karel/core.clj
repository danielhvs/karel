(ns karel.core
  (:require
    [clojure.string :as str]))

(defn entity [x y t]
  {:x x :y y :type t})

(defn karel [x y]
  (entity x y :karel))

(defn wall [x y]
  (entity x y :wall))

(defn walk [entity]
  (update entity :x inc))

(defn step "state is a vector of entities"
  [state]
  (if-let [karel (first
                   (filter
                     #(= (get-in % [:type]) :karel)
                     state))]
    (walk karel)
    state))

(comment
  (step [(wall 0 0)])
  (step [(karel 1 0) (wall 1 1)]))
