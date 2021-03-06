;; ideas:
;; 
;; - buy upgrades with energy:
;;   more energy per enemy drop,
;;   faster movement (e.g. frequent enemy pause),
;;   more enemy spawns (less cooldown between spawns)
;;   
;; - purchase field effects from energy:
;;   stop spawning on field (e.g. to block corners),
;;   increased chance of spawning towards field (e.g. to lure into center),
;;   increase (enemy) energy when stepping on field (feed),
;;   change direction when stepping on field,
;;   pause for n steps when stepping on field (trap),
;; 
;; - establish 'drop zones', e.g. enemies caught near center are worth less (drop less energy) than enemies caught around the edge of the map
;; 
;; - [x] plot the delta between the steps taken & the energy earned by the next collected enemy;
;;   [ ] assign bonuses / maluses for chains (3 positive budgets in a row, 3 negative budgets in a row)
;; 
;; - chain bonuses: for every consecutive positive ledger, add 1 to collected energy.
;;   think: does ledger then take increased energy into account when tallying next collection? (makes chaining much easier once chain has started)
;;   
;; - instead of summing energy, make movement free but start a chain countdown when collecting.
;;   chain length can then be used to buy updates (a la gradius).
;;   'repair tile' is also an upgrade.
;;   'increase chain time' as an upgrade.
;;   
;; - [x] increase difficulty by removing floors (so player can't move on it, enemies die on it)

(ns fig-dungeon.core
  (:require [clojure.set :refer [difference union]]
            [reagent.core :as reagent :refer [atom]]
            [fig-dungeon.common :refer [gridsize
                                        random
                                        out-of-bounds?
                                        tile-index
                                        floor-exists?]]
            [fig-dungeon.enemies :refer [move-enemies
                                         create-enemy
                                         spawn-enemy
                                         collect-enemy
                                         remove-dead-enemies
                                         kill-out-of-bound-enemies]]))

(enable-console-print!)

(def initial-state {:enemies []
                    :moves 0
                    :notice false
                    :picking false
                    :pick-fn (fn [x y] nil)
                    :floor (set (for [y (range gridsize)
                                      x (range gridsize)]
                                  {:x x :y y
                                   :id (tile-index x y)}))
                    :ledger {:deltas [] ; ledger of collections.
                             :last-catch 0 ; move # when player last collected energy.
                             }
                    :chain {:count 0 ; current chain count.
                            :left 0 ; moves left in current chain.
                            :time 5 ; moves to continue chain (one
                                    ; more than intended because chain
                                    ; time is immediately decremented
                                    ; after player's collecting move).
                            }
                    :player {:energy 5
                             :x (.floor js/Math (/ gridsize 2))
                             :y (.floor js/Math (/ gridsize 2))}})

(defonce app-state (atom initial-state))

(defn remove-floor []
  (when (= 0 (mod (:moves @app-state) 14))
    (let [floor (:floor @app-state)
          pick (set (vector (nth (seq floor)
                                 (random (dec (count floor))))))]
      ;;(println "removing floor" floor pick)
      (swap! app-state
             assoc :floor
             (difference floor pick))
      ;;(println "tile count" (count (:floor @app-state)))
      )))

(defn dec-chain-time []
  (swap! app-state update-in [:chain :left] #(max 0 (dec %))))

(defn break-chain []
  (swap! app-state
         (fn [s]
           (if (= 0 (-> s :chain :left))
             (assoc-in s [:chain :count] 0)
             s))))

(defn opponents-move []
  "called after the player moves."
  (remove-dead-enemies app-state)
  (collect-enemy app-state)
  (move-enemies app-state)
  (collect-enemy app-state)
  (dec-chain-time)
  (break-chain) ; chains are broken on "opponents" move because that's where collection happens.
  (spawn-enemy app-state)
  (remove-floor)
  (kill-out-of-bound-enemies app-state))

(defn repair-tile-picked [x y]
  (let [tile {:x x :y y
              :id (tile-index x y)}]
    (when (and (not ((:floor @app-state) tile))
               (not (out-of-bounds? tile)))
      (swap! app-state
             #(-> %
                  (assoc :picking false)
                  (update :floor
                          union (set [{:x x :y y
                                       :id (tile-index x y)}]))
                  (update-in [:player :energy] dec)
                  (update :moves inc)))
      (opponents-move))))

(defn repair-tile []
  (when (< (count (:floor @app-state))
           (* gridsize gridsize))
    (swap! app-state
           assoc
           :picking true
           :pick-fn #'repair-tile-picked)))

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
              (floor-exists? app-state x y))
     (swap!
      app-state
      (fn [s]
        (-> s
            (update :player assoc :x x :y y)
            (update-in [:player :energy] dec)
            (update :moves inc))))
     (opponents-move))))

(defn position-style [thing]
  {:top (* 50 (:y thing))
   :left (* 50 (:x thing))})

(defn init []
  (reset! app-state initial-state)
  (spawn-enemy app-state))

(init)

(defn key-down [e]
  (case (.-key e)
    "r" (init)
    "w" (move-player :up)
    "a" (move-player :left)
    "s" (move-player :down)
    "d" (move-player :right)
    nil))

(defn pick-touch [t]
  (let [touch (-> t
                  (.-changedTouches)
                  (.item 0))
        exp #(.floor js/Math (/ % 50))
        x (exp (.-pageX touch))
        y (exp (.-pageY touch))]
    ;;(println "touching at" x y)
    ((:pick-fn @app-state) x y)))

(defn pick-click [c]
  (let [exp #(.floor js/Math (/ % 50))
        x (exp (.-pageX c))
        y (exp (.-pageY c))]
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
   "chain:" (str (-> @app-state :chain :count)) [:br]
   "moves:" (str (:moves @app-state)) [:br]
   "last catch:" (str (-> @app-state :ledger :last-catch)) [:br]
   "history:" (str (-> @app-state :ledger :deltas))])

(defn mobile-controls []
  [:div.controls
   (doall
    (for [dir '(:up :right :left :down)]
      ^{:key dir}
      [:div.button
       {:on-touch-start (fn [e]
                          (.preventDefault e)
                          (.stopPropagation e)
                          (move-player dir))
        :on-mouse-down (fn [e]
                         (.preventDefault e)
                         (.stopPropagation e)
                         (move-player dir))}]))])

(defn counter
  [n
   &{:keys [color
            center]
     :or {color "green"
          center false}}]
  [:div.counter
   (doall
    (for [m (range n)]
      ^{:key m}
      [:div.ball {:style {:background-color color}}]))
   [:br {:style {:clear "both"}}]])

(defn chain-notice []
  (fn []
    (when (:notice @app-state)
      (js/setTimeout #(swap! app-state assoc :notice false) 1))
    [:div.chain
     {:class (if (:notice @app-state) nil "off")}
     ;;(str (last (-> @app-state :ledger :deltas)))
     [counter (-> @app-state :chain :count)
      :color "black"
      :center true]]))

(defn smallest-fit
  ([n]
   (smallest-fit n 1))
  ([n i]
   (if (> (* i i) n) i (smallest-fit n (inc i)))))

(defn energy-container-size []
  (* 25 (smallest-fit (-> @app-state :player :energy))))

(defn hello-world []
  [:div.main 
   {:tab-index 1             ; enables focus, to catch keyboard events
    :on-key-down #'key-down}
   [:div.field
    [player]
    [enemies]
    [grid]]
   ;; GUI
   ;; [:div.energy
   ;;  {:style {:width (* 20 (-> @app-state :player :energy))}}]
   [counter (-> @app-state :player :energy) :color "green"]
   [counter (-> @app-state :chain :count) :color "black"]
   [counter (-> @app-state :chain :left) :color "red"]
   [chain-notice]
   [mobile-controls]
   ;; temporary overlay
   [:div.pick-overlay
    {:class (if (:picking @app-state) "show" "hidden")
     :on-touch-start #'pick-touch
     :on-click #'pick-click}
    "PICK TILE TO REPAIR"]
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
