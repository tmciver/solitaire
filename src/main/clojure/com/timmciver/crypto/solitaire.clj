(ns com.timmciver.crypto.solitaire
  (:use [clojure.contrib.def :only (defvar)]
        [clojure.string :only (upper-case)]))

(defvar jokerA 53)
(defvar jokerB 54)

(defn joker?
  "Returns true if the card is a joker (53 or 54), false otherwise."
  [card]
  {:pre [(<= card 54)]}
  (or (= card jokerA) (= card jokerB)))

(defn ordered-deck
  "Returns an ordered deck (integers 1 to 54)."
  []
  (range 1 55))

(defn random-deck
  "Returns a sequence of integers from 1 to 54 in a random order."
  []
  (shuffle (ordered-deck)))

(defn number-to-letter
  "Generates a seq of letters from a seq of integers.  The integers
can have any values; a modulo 26 is performed putting all integers in the
range from 1 to 26 which then maps to A through Z."
  [ints]
  (map #(char (+ (mod (- % 1) 26) 65)) ints))

(defn letter-to-number
  "Generates a seq of integers from a seq of characters (string)
where 1 corresponds to 'a' and 26 corresponds to 'z'. Accepts both upper
and lower case letters.  Output is given for letters only, not whitespace."
  [s]
  (filter #(and (> % 0) (< % 27)) (map #(- (int %) 64) (upper-case s))))

(defn move-card-down
  "With two arguments, moves the given card one space closer to the bottom of
the deck. With three arguments, moves the given card the given number of spaces
toward the bottom of the deck. If card moves past bottom of deck, it cycles back
to the top of the deck. deck must consist of integers from 1 to 54 in any
order."
  ([deck card]
     {:pre [(<= card 54) (= (count deck) 54)]}
     (let [vdeck (vec deck)
           old-pos (.indexOf vdeck card)]
       (if (< old-pos (dec (count vdeck)))
         (let [nval (get vdeck (inc old-pos))]
           (assoc (assoc vdeck old-pos nval) (inc old-pos) card))
         (concat (vector (first vdeck)) (vector card) (butlast (rest vdeck))))))
  ([deck card spaces]
     {:pre [(<= card 54) (= (count deck) 54)]}
     (if (zero? spaces)
       deck
       (recur (move-card-down deck card) card (dec spaces)))))

(defn split-at-jokers
  "Returns a three-element vector whose first element is the chunk of cards
before the first joker, whose second element is the chunk of cards starting
with the first joker followed by all the cards after the first joker but
before the second joker, and finally whose third element is the chunk of
cards starting with the second joker followed by the rest of the cards in
the deck."
  [deck]
  {:pre [(= (count deck) 54)]}
  (let [[top therest] (split-with #(not (joker? %)) deck)
        [middle bottom] (split-with #(not (joker? %)) (rest therest))]
    [top (cons (first therest) middle) bottom]))

(defn triple-cut
  "Performs a triple cut around the two Jokers (cards 53 and 54):
the chunk of cards above the first Joker swaps places with the chunk
of cards below the second Joker."
  [deck]
  (let [[top middle bottom] (split-at-jokers deck)]
    (concat (rest bottom) (concat middle (list (first bottom))) top)))

(defn cut-preserve-bottom
  "Returns a new deck which is the result of performing a cut at position
and leaving the bottom card in place."
  [deck position]
  (let [[top therest] (split-at position deck)
        [middle bottom] (split-at (dec (count therest)) therest)]
    (concat middle top bottom)))

(defn count-cut
  "Returns a new deck which is the result of performing a cut at the value
of the last card (a joker is 53) but the last card stays in place."
  [deck]
  (let [bottomcard (if (joker? (last deck)) 53 (last deck))]
    (cut-preserve-bottom deck bottomcard)))

(defn solitaire
  "Performs one iteration of the solitaire algorithm on deck and returns
the modified deck. deck is a sequence of integers from 1 to 54 in any order."
  [deck]
  (-> deck
      (move-card-down jokerA)
      (move-card-down jokerB 2)
      triple-cut
      count-cut))

;; (defn key-deck
;;   "Uses the given passphrase to key deck using the solitaire algorithm."
;;   [deck passphrase]
;;   (letfn [(keyfn [card]
;;             (-> deck
;;                 solitaire
;;                 (cut-preserve-bottom card)))]
;;     (map (keyfn %) (to-int-stream passphrase))))

(defn key-deck
  "Uses the given passphrase to key deck using the solitaire algorithm."
  [deck passphrase]
  (if (empty? passphrase)
    deck
    (let [char-val (first (letter-to-number passphrase))
          new-deck (-> deck
                       solitaire
                       (cut-preserve-bottom char-val))]
      (key-deck new-deck (next passphrase)))))

(defn generate-key
  "Takes a deck on which the solitaire algorithm has been performed and returns
the next valid key (performing the solitaire algorith again, if a joker is
  returned."
  [deck]
  (let [[top bottom] (split-at (first deck) deck)
        key (first bottom)]
    (if (joker? key)
      (generate-key (solitaire deck))
      key)))

(defn key-card
  "Returns the key card from the given deck."
  [deck]
  (let [[top bottom] (split-at (first deck) deck)]
    (first bottom)))

(defn solitaire-keystream
  "An infinite sequence of keys generated by performing the solitaire
algorithm on the given deck repeatedly producing a key each time."
  [deck]
  (let [key (generate-key deck)]
    (lazy-seq
     (cons key (solitaire-keystream (solitaire deck))))))

(defn encode
  "Encrypts the message text by performing the solitaire algorithm on the
given deck. Returns a vector containing values representing the cipher."
  [message deck]
  (let [key-stream (solitaire-keystream deck)
        message-vals (letter-to-number message)
        encoded-vals (->> (map #(+ %1 %2) key-stream message-vals)
                          (map #(mod % 26)))]
    (apply str (number-to-letter encoded-vals))))