(ns Player
    (:require [clojure.pprint :refer [pprint]]
      [clojure.string :refer [split]])
    (:gen-class))

(def turn (atom 0))

(defn get-phase [turn]
      (if (<= turn 30)
        :draft
        :battle))

(defn get-my-cards [cards]
      (filter #(= (:location %) 1) cards))

(defn get-my-cards-in-hand [cards]
      (filter #(= (:location %) 0) cards))

(defn get-opponent-cards [cards]
      (filter #(= (:location %) -1) cards))

(defn draft-action [cards] "PASS")

(defn battle-action
      [state]
      (let [{:keys [my opponent cards]} state
            my-cards                    (get-my-cards cards)
            my-cards-in-hand            (sort-by :cost (get-my-cards-in-hand cards))]
           (if (empty? my-cards)
             ; attempt to summon
             (if (>= (my :mana) (:cost (first my-cards-in-hand)))
               (str "SUMMON " (:instanceId (first my-cards-in-hand)))
               "PASS")
             ; or attack
             (str "ATTACK " (:instanceId (first my-cards)) " -1"))))

(defn get-action [phase state]
      (let [{:keys [cards]} state]
           ;   (binding [*out* *err*]
           ;             (pprint (get-my-cards cards)))
           (if (= phase :draft)
             (draft-action cards)
             (battle-action state))))

(defn parse-abilities [raw]
      (split raw #""))

(defn read-player [line]
      (zipmap [:health :mana :deck :rune]
              (map #(Integer/parseInt %) line)))

; todo: use destructuring instead of zipmap?
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
      (let [{:keys [cardCount] :as player-data} {:my           (read-player             (nth lines 0))
                                                 :opponent     (read-player             (nth lines 1))
                                                 :opponentHand (Integer/parseInt (first (nth lines 2)))
                                                 :cardCount    (Integer/parseInt (first (nth lines 3)))}
            cards (->> lines
                       (drop 4)
                       (take cardCount)
                       (map read-card))]
           (assoc player-data :cards cards)))

(defn -main [& args]
      (while true
             (swap! turn inc)
             (let [lines (map #(split % #"\s") (line-seq (java.io.BufferedReader. *in*)))
                   state (read-state lines)
                   phase (get-phase @turn)
                   action (get-action phase state)]
                  ;  (binding [*out* *err*]
                  ;         (pprint state))
                  ; Write action to stdout
                  (println action))))
