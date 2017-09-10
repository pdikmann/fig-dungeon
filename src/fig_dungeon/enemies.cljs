(ns fig-dungeon.enemies
  (:require [fig-dungeon.common :refer [gridsize
                                        energy-bonus
                                        random-dir
                                        random
                                        my-gensym
                                        at-same-position?
                                        out-of-bounds?
                                        floor-exists?]]))

(defn- move-enemy [e]
  (if (:dead e)
    e
    (case (:dir e)
      :up (update e :y dec)
      :down (update e :y inc)
      :left (update e :x dec)
      :right (update e :x inc))))

(defn move-enemies [app-state]
  (swap! app-state
         update :enemies
         #(mapv #'move-enemy %)))

(defn create-enemy [app-state]
  "introduce one new enemy around the edge of the playing field."
  (let [dir (random-dir)
        max (dec gridsize)
        pos (random max)
        nme (case dir
              :up    {:x pos :y max}
              :down  {:x pos :y 0}
              :left  {:x max :y pos}
              :right {:x 0 :y pos})]
    (if (at-same-position? nme (:player @app-state))
      (recur app-state)
      (assoc nme
             :dir dir
             :id (my-gensym)
             :dead false))))

(defn spawn-enemy [app-state]
  "spawn an enemy in regular intervals (every n turns)."
  (when (= 0 (mod (:moves @app-state) 3))
    (swap! app-state
           update :enemies
           #(conj % (create-enemy app-state)))))

(defn collect-enemy [app-state]
  "kill enemies at player position and increase player energy."
  (let [player-pos (:player @app-state)
        at-player-position? #(at-same-position? % player-pos)
        collection (->> (:enemies @app-state)
                        (remove #(:dead %))
                        (filter at-player-position?))
        energy (* energy-bonus (count collection))
        ]
    (when (not (empty? collection))
      (swap! app-state
             (fn [s]
               (-> s
                   ;; player collects energy.
                   (update-in [:player :energy]
                              #(+ % energy))
                   ;; enemies are tagged 'dead'.
                   (update :enemies
                           (fn [es]
                             (mapv (fn [e]
                                     (if (at-player-position? e)
                                       (assoc e :dead true)
                                       e))
                                   es)))
                   ;; collection history is updated.
                   (update :ledger
                           (fn [l]
                             (let [moves (- (:moves @app-state)
                                            (:last-catch l))
                                   delta (- energy moves)]
                               (-> l
                                   (update :deltas conj delta)
                                   (assoc :last-catch (:moves @app-state))))))
                   (assoc :notice true)))))))

(defn remove-dead-enemies [app-state]
  (swap! app-state
         update :enemies
         (fn [es] (vec (remove #(:dead %) es)))))

(defn kill-out-of-bound-enemies [app-state]
  (swap! app-state
         update :enemies
         (fn [es]
           (mapv #(if (or (out-of-bounds? %)
                          (not (floor-exists? app-state %)))
                    (assoc % :dead true)
                    %)
                 es))))
