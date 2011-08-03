(ns com.timmciver.crypto.solitaire)

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
  (let [[top therest] (split-with #(not= % card) deck)
        [middle bottom] (split-at spaces (next therest))]
    (concat top middle (list card) bottom)))