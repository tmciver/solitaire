(ns com.timmciver.crypto.solitaire
  (:use [clojure.contrib.seq :only (positions)]))

(defn solitaire
  [deck]
  "Performs one iteration of the solitaire algorithm on deck
and returns the modified deck. deck is a sequence of integers
from 1 to 54 in any order."
  )

(defn move-card-down
  [deck card spaces]
  "Move card card down spaces spaces in deck. If card moves past
bottom of deck, it cycles back to the top of the deck."
  {:pre [(<= card 54) (<= (count deck) 54)]}
  (let [old-pos (.indexOf (vec deck) card)
        new-pos (mod (+ old-pos spaces) (dec (count deck)))
        [top therest] (split-at new-pos (remove #(= % card) deck))]
    (concat top (list card) therest)))

(defn triple-cut
  [deck]
  "Performs a triple cut around the two Jokers (cards 53 and 54):
the chunk of cards above the first Joker swaps places with the chunk
of cards below the second Joker."
  (let [joker? (fn [card] (or (= card 53) (= card 54)))
        [top therest] (split-with #(not (joker? %)) deck)
        [rbottom rmiddle] (split-with #(not (joker? %)) (reverse therest))]
    (concat (reverse rbottom) (reverse rmiddle) top)))