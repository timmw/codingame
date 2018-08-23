(ns Player
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :refer [split]]
            [clojure.spec.alpha :as s])
  (:gen-class)
  (:import (java.io BufferedReader)))

; ---------------------------------------------------------------------------
; Constraints
; ---------------------------------------------------------------------------

(def max-cards-per-side 6)

(def location-map {"-1" :opponent-side
                   "0"  :my-hand
                   "1"  :my-side})

(def card-properties
  [:cardNumber
   :instanceId
   :location
   :cardType
   :cost
   :attack
   :defense
   :abilities
   :myHealthChange
   :opponentHealthChange
   :cardDraw
   ])

; ---------------------------------------------------------------------------
; Input processing
; ---------------------------------------------------------------------------

(defn parse-abilities [raw]
  (split raw #""))

(defn read-player [line]
  (zipmap [:health :mana :deck :rune]
          (map #(Integer/parseInt %) line)))

(defn read-card [line]
  (zipmap [:cardNumber
           :instanceId
           :location
           :cardType
           :cost
           :attack
           :defense
           :abilities
           :myHealthChange
           :opponentHealthChange
           :cardDraw]
          (map #(try (Integer/parseInt %)
                     ; abilities is the only data point which is not an int
                     (catch java.lang.NumberFormatException e
                       (parse-abilities %)))
               line)))

(defn read-state [lines]
  (let [{:keys [cardCount] :as player-data} {:my-stats     (read-player             (nth lines 0))
                                             :opponent     (read-player             (nth lines 1))
                                             :opponentHand (Integer/parseInt (first (nth lines 2)))
                                             :cardCount    (Integer/parseInt (first (nth lines 3)))}
        cards (->> lines
                   (drop 4)
                   (take cardCount)
                   (map read-card))]
    (assoc player-data :cards cards)))

(defn get-phase [turn]
  (if (<= turn 30)
    :draft
    :battle))

(defn filter-by-location [location cards]
  (filter #(= (:location %) location) cards))

(defn get-my-cards [cards]
  (filter #(= (:location %) 1) cards))

(defn get-my-cards-in-hand [cards]
  (filter #(= (:location %) 0) cards))

(defn get-opponent-cards [cards]
  (filter #(= (:location %) -1) cards))

; ---------------------------------------------------------------------------
; Game logic
; ---------------------------------------------------------------------------

; Draft

(defn draft-action-simplest [cards]
  "PASS")

(defn draft-action-lowest-cost [cards]
  (let [cheapest-card (->> cards
                           (map-indexed vector)
                           (sort-by #(:cost (second %)))
                           ffirst)]
    (str "PICK " cheapest-card)))


; Battle

(defn should-summon? [my-cards my-cards-in-hand my-stats]
  ; (< (count my-cards) max-cards-per-side)
  ; (> (my-stats :mana) (first (sort-by :cost my-cards-in-hand))
  )

(defn battle-action
  [state]
  (let [{:keys [my-stats opponent cards]} state
        my-cards                    (get-my-cards cards)
        my-cards-in-hand            (sort-by :cost (get-my-cards-in-hand cards))]
    (if (empty? my-cards)
      ; attempt to summon
      (if (>= (my-stats :mana) (:cost (first my-cards-in-hand)))
        (str "SUMMON " (:instanceId (first my-cards-in-hand)))
        "PASS")
      ; or attack
      (str "ATTACK " (:instanceId (first my-cards)) " -1"))))

; Action
; phase :draft | :battle
; state
(defn get-action [phase state]
  (let [{:keys [cards]} state]
    (binding [*out* *err*] (pprint state))
    (if (= phase :draft)
      (draft-action-lowest-cost cards)
      (battle-action state))))

; ---------------------------------------------------------------------------
; Main loop
; ---------------------------------------------------------------------------

(defn -main [& _]
  (loop [turn 1]
    (let [lines (->> (BufferedReader. *in*)
                     line-seq
                     (map #(split % #"\s")))
          state (read-state lines)
          phase (get-phase turn)
          action (get-action phase state)]

      (println action)
      (recur (inc turn)))))
