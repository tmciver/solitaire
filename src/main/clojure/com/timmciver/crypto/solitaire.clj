(ns com.timmciver.crypto.solitaire)

(defn solitaire
  [deck]
  "Performs one iteration of the solitaire algorithm on deck
and returns the modified deck. deck is a sequence of integers
from 1 to 54 in any order."
  )

(defn random-deck
  []
  "Returns a sequence of integers from 1 to 54 in a random order."
  (shuffle (range 1 55)))

(defn move-card-down
  [deck card spaces]
  "Move card card down spaces spaces in deck. If card moves past
bottom of deck, it cycles back to the top of the deck. deck must
consist of integers from 1 to 54 in any order."
  {:pre [(<= card 54) (= (count deck) 54)]}
  (let [old-pos (.indexOf (vec deck) card)
        new-pos (mod (+ old-pos spaces) (dec (count deck)))
        [top therest] (split-at new-pos (remove #(= % card) deck))]
    (concat top (list card) therest)))

(defn joker?
  [card]
  "Returns true if the card is a joker (53 or 54), false otherwise."
  {:pre [(<= card 54)]}
  (or (= card 53) (= card 54)))

(defn split-at-jokers
  [deck]
  "Returns a three-element vector whose first element is the chunk of cards
before the first joker, whose second element is the chunk of cards starting
with the first joker followed by all the cards after the first joker but
before the second joker, and finally whose third element is the chunk of
cards starting with the second joker followed by the rest of the cards in
the deck."
  {:pre [(= (count deck) 54)]}
  (let [[top therest] (split-with #(not (joker? %)) deck)
        [middle bottom] (split-with #(not (joker? %)) (rest therest))]
    [top (cons (first therest) middle) bottom]))

(defn triple-cut
  [deck]
  "Performs a triple cut around the two Jokers (cards 53 and 54):
the chunk of cards above the first Joker swaps places with the chunk
of cards below the second Joker."
  (let [[top middle bottom] (split-at-jokers deck)]
    (concat (rest bottom) (concat middle (list (first bottom))) top)))