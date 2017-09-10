;; ideas:
;; - buy upgrades with energy:
;;   more energy per enemy drop,
;;   faster movement (e.g. frequent enemy pause),
;;   more enemy spawns (less cooldown between spawns)
;; - purchase field effects from energy:
;;   stop spawning on field (e.g. to block corners),
;;   increased chance of spawning on field (e.g. to lure into center),
;;   increase (enemy) energy when stepping on field (feed),
;;   change direction when stepping on field,
;;   pause for n steps when stepping on field (trap),
;; - increase difficulty by removing floors (so player can't move on it, enemies die on it)
;; - establish 'drop zones', e.g. enemies caught near center are worth less (drop less energy) than enemies caught around the edge of the map
;; - plot the delta between the steps taken & the energy earned by the next collected enemy; assign bonuses / maluses for chains (3 positive budgets in a row, 3 negative budgets in a row)
(ns fig-dungeon.core
  (:require [clojure.set :refer [difference union]]
            [reagent.core :as reagent :refer [atom]]
            [fig-dungeon.common :refer [gridsize
                                        random-dir
                                        random
                                        at-same-position?
                                        out-of-bounds?
                                        tile-index]]))

(enable-console-print!)

(def energy-bonus 4)

(def initial-state {:enemies []
                    :moves 0
                    :picking false
                    :pick-fn (fn [x y] nil)
                    :floor (set (for [y (range gridsize)
                                      x (range gridsize)]
                                  {:x x :y y
                                   :id (tile-index x y)}))
                    :ledger {:deltas [] ; ledger of collections
                             :last-catch 0 ; move # when player last collected energy
                             }
                    :player {:energy 5
                             :x (.floor js/Math (/ gridsize 2))
                             :y (.floor js/Math (/ gridsize 2))}})

(defonce app-state (atom initial-state))

(def counter 0)

(defn my-gensym []
  (set! counter (inc counter)))

(defn remove-floor []
  (when (= 0 (mod (:moves @app-state) 3))
    (let [floor (:floor @app-state)
          pick (set (vector (nth (seq floor)
                                 (random (dec (count floor))))))]
      ;;(println "removing floor" floor pick)
      (swap! app-state
             assoc :floor
             (difference floor pick))
      ;;(println "tile count" (count (:floor @app-state)))
      )))

(defn floor-exists?
  ([{:keys [x y]}]
   (floor-exists? x y))
  ([x y]
   ((:floor @app-state) {:x x :y y
                         :id (tile-index x y)})))

(defn repair-tile-picked [x y]
  (swap! app-state
         #(-> %
           (update :floor
                   union (set [{:x x :y y
                                :id (tile-index x y)}]))
           (update-in [:player :energy] dec)))
  ;;(opponents-move)
  )

(defn repair-tile []
  (swap! app-state
         assoc
         :picking true
         :pick-fn #'repair-tile-picked))

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
        collection (->> (:enemies @app-state)
                        (remove #(:dead %))
                        (filter at-player-position?))
        energy (* energy-bonus (count collection))
        ]
    (when (not (empty? collection))
      (swap! app-state
             (fn [s]
               (-> s
                   (update-in [:player :energy]
                              #(+ % energy))
                   (update :enemies
                           (fn [es]
                             (mapv (fn [e]
                                     (if (at-player-position? e)
                                       (assoc e :dead true)
                                       e))
                                   es)))
                   (update :ledger
                           (fn [l]
                             (let [moves (- (:moves @app-state)
                                            (:last-catch l))
                                   delta (- energy moves)]
                               (-> l
                                   (update :deltas conj delta)
                                   (assoc :last-catch (:moves @app-state))))))
                   ))))))

(defn remove-dead-enemies []
  (swap! app-state
         update :enemies
         (fn [es] (vec (remove #(:dead %) es)))))

(defn kill-out-of-bound-enemies []
  (swap!
   app-state
   update :enemies
   (fn [es]
     (mapv #(if (or (out-of-bounds? %)
                    (not (floor-exists? %)))
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
  (remove-floor)
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
   (when (and (< 0 (-> @app-state :player :energy))
              (not (out-of-bounds? {:x x :y y}))
              (floor-exists? x y))
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

(defn init []
  (reset! app-state initial-state)
  (spawn-enemy))

(defn key-down [e]
  (case (.-key e)
    "r" (init)
    "w" (move-player :up)
    "a" (move-player :left)
    "s" (move-player :down)
    "d" (move-player :right)
    nil))

(defn pick-touch [t]
  (swap! app-state
         assoc :picking false)
  (let [touch (-> t
                  (.-changedTouches)
                  (.item 0))
        exp #(.floor js/Math (/ % 50))
        x (exp (.-pageX touch))
        y (exp (.-pageY touch))]
    ;;(println "touching at" x y)
    ((:pick-fn @app-state) x y)))

;; --------------------------------------------------------------------------------
;; react
(defn player []
  [:div.player
     {:style (position-style (:player @app-state))}])

(defn enemy [e]
  [:div.enemy {:style (position-style e)
               :class (if (:dead e) "dead" nil)}])

(defn enemies []
  [:div
   (doall
     (for [e (:enemies @app-state)]
       ^{:key (:id e)}
       [enemy e]))])

(defn grid []
  [:div
   (doall
    (for [f (:floor @app-state)]
      ^{:key (:id f)}
      [:div.block {:style {:top (* 50 (:y f))
                           :left (* 50 (:x f))}}]))])

(defn debug []
  [:p
   "energy:" (str (-> @app-state :player :energy)) [:br]
   "moves:" (str (:moves @app-state)) [:br]
   "last catch:" (str (-> @app-state :ledger :last-catch)) [:br]
   "history:" (str (-> @app-state :ledger :deltas))])

(defn hello-world []
  [:div.main 
   {:tab-index 1             ; enables focus, to catch keyboard events
    :on-key-down #'key-down}
   [:div.field
    [player]
    [enemies]
    [grid]]
   ;; GUI
   [:div.energy
    {:style {:width (* 20 (-> @app-state :player :energy))}}]
   ;; Mobile Controls
   [:div.controls
    (doall
     (for [dir '(:up :right :left :down)]
       ^{:key dir}
       [:div.button
        {:on-touch-start (fn [e]
                           (.preventDefault e)
                           (.stopPropagation e)
                           (move-player dir))}]))]
   ;; temporary overlay
   [:div.pick-overlay
    {:class (if (:picking @app-state) "show" "hidden")
     :on-touch-start #'pick-touch}]
   ;; "Game Over"-Notice
   [:div.game-over
    {:class (if (= 0 (-> @app-state :player :energy)) "show" "hidden")}
    [:a {:href "#" :on-click #'init}
     "GAME OVER"]]
   ;; Build Orders
   [:div.builds
    [:a {:href "#"
         :on-click #'repair-tile}
     "REPAIR TILE"]]
   ;; Debug
   [debug]
   ])

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
