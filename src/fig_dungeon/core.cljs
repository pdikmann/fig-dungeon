;; ideas:
;; - buy upgrades with energy: more energy per enemy drop, faster movement (e.g. frequent enemy pause), more enemy spawns (less cooldown between spawns)
;; - increase difficulty by removing floors (so player can't move on it, enemies die on it)
;; - establish 'drop zones', e.g. enemies caught near center are worth less (drop less energy) than enemies caught around the edge of the map
;; - plot the delta between the steps taken & the energy earned by the next collected enemy; assign bonuses / maluses for chains (3 positive budgets in a row, 3 negative budgets in a row)

(ns fig-dungeon.core
  (:require [reagent.core :as reagent :refer [atom]]
            [fig-dungeon.common :refer [gridsize
                                        random-dir
                                        random
                                        at-same-position?
                                        out-of-bounds?]]))

(enable-console-print!)

(def initial-state {:enemies []
                    :moves 0
                    :player {:energy 10
                             :x 4 :y 4}})

(defonce app-state (atom initial-state))

(def counter 0)

(defn my-gensym []
  (set! counter (inc counter)))

(defn init []
  (reset! app-state initial-state))

(defn move-enemy [e]
  (if (:dead e)
    e
    (case (:dir e)
      :up (update e :y dec)
      :down (update e :y inc)
      :left (update e :x dec)
      :right (update e :x inc))))

(defn move-enemies []
  (swap! app-state
         update :enemies
         #(mapv #'move-enemy %)))

(defn create-enemy []
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
      (recur)
      (assoc nme
             :dir dir
             :id (my-gensym)
             :dead false))))

(defn spawn-enemy []
  "spawn an enemy in regular intervals (every n turns)."
  (when (= 0 (mod (:moves @app-state) 3))
    (swap! app-state
           update :enemies
           #(conj % (create-enemy)))))

(defn collect-enemy []
  "kill enemies at player position and increase player energy."
  (let [player-pos (:player @app-state)
        at-player-position? #(at-same-position? % player-pos)
        collection (filter at-player-position? (:enemies @app-state))
        ]
    (when (not (empty? collection))
      (swap! app-state
             (fn [s]
               (-> s
                   (update-in [:player :energy] #(+ % (* 4 (count collection))))
                   (update :enemies (fn [es]
                                      (mapv (fn [e]
                                              (if (at-player-position? e)
                                                (assoc e :dead true)
                                                e))
                                            es)))))))))

(defn remove-dead-enemies []
  (swap! app-state
         update :enemies
         (fn [es] (vec (remove #(:dead %) es)))))

(defn kill-out-of-bound-enemies []
  (swap!
   app-state
   update :enemies
   (fn [es]
     (mapv #(if (out-of-bounds? %)
              (assoc % :dead true)
              %)
           es))))

(defn opponents-move []
  "called after the player moves."
  (remove-dead-enemies)
  (collect-enemy)
  (move-enemies)
  (collect-enemy)
  (spawn-enemy)
  (kill-out-of-bound-enemies))

(defn move-player 
  ([dir]
   (let [{:keys [x y]} (:player @app-state)]
     (case dir
       :up (move-player x (dec y))
       :down (move-player x (inc y))
       :left (move-player (dec x) y)
       :right (move-player (inc x) y))))
  ([x y]
   (when (and ;;(-> @app-state :player :active)
          (< 0 (-> @app-state :player :energy))
          (not (out-of-bounds? {:x x :y y})))
     ;;(collect-enemy)
     (swap!
      app-state
      #(-> %
           (update :player assoc :x x :y y)
           (update-in [:player :energy] dec)
           (update :moves inc)))
     (opponents-move))))

(defn position-style [thing]
  {:top (* 50 (:y thing))
   :left (* 50 (:x thing))})

(defn key-down [e]
  (case (.-key e)
    "r" (init)
    "w" (move-player :up)
    "a" (move-player :left)
    "s" (move-player :down)
    "d" (move-player :right)
    nil))

(defn hello-world []
  [:div.main 
   {:tab-index 1 ; enables focus, to catch keyboard events
    :on-key-down #'key-down}
   [:div.field
    ;; Player
    [:div.player
     {:style (position-style (:player @app-state))}]
    ;; Enemies
    (doall
     (for [e (:enemies @app-state)]
       ^{:key (:id e)}
       [:div.enemy {:style (position-style e)
                    :class (if (:dead e)
                             "dead"
                             nil)}]))
    ;; Grid
    (for [y (range gridsize)
          x (range gridsize)]
      ^{:key (+ x (* y gridsize))}
      [:div.block])]
   [:br {:style {:clear "both"}}]
   ;; GUI
   [:div.energy
    {:style {:width (* 20 (-> @app-state :player :energy))}}]
   [:p
    "energy:" (str (-> @app-state :player :energy)) [:br]
    "moves:" (str (:moves @app-state)) [:br]]
   ;; Mobile Controls
   [:div.controls
    (for [dir '(:up :right :left :down)]
      ^{:key dir}
      [:div.button
       {:on-touch-start (fn [e]
                          (.preventDefault e)
                          (.stopPropagation e)
                          (move-player dir))}])]
   ])

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
