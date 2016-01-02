(ns com.timmciver.crypto.solitaire
  (:use [clojure.string :only (upper-case)]))

(def jokerA 53)
(def jokerB 54)
(def ordered-deck (range 1 55))

(defn- valid-card?
  "Returns true if the given value represents a valid card."
  [card]
  (and (integer? card)
       (< 0 card 55)))

(defn- valid-deck?
  "Returns true if the given value is a collection of 54 valid cards;
  false otherwise."
  [deck]
  (and (= (count deck) 54)
       (every? valid-card? deck)))

(defn- valid-char?
  "Returns true if the given character is alphabetic, false otherwise."
  [chr]
  (or (Character/isUpperCase chr) (Character/isLowerCase chr)))

(defn- valid-string?
  "Returns true if the given string contains only alphabetic characters, false
  otherwise."
  [str]
  (every? valid-char? str))

(defn- pad-to-mod-5-with-x
  "Returns a string that is the given string padded with \"X\" so that its
  length is an integer number of 5 characters."
  [s]
  (let [mod5 (mod (count s) 5)
        num-pads (if (zero? mod5) 0 (- 5 mod5))]
    (apply str (concat s (repeat num-pads \X)))))

(defn joker?
  "Returns true if the card is a joker (53 or 54), false otherwise."
  [card]
  {:pre [(valid-card? card)]}
  (or (= card jokerA) (= card jokerB)))

(defn random-deck
  "Returns a sequence of integers from 1 to 54 in a random order."
  []
  (shuffle ordered-deck))

(defn number-to-letter
  "Returns the upper-case character that is represented by the given integer
  where the integers from 1 to 26 map to characters A through Z."
  [num]
  {:pre [(< 0 num 27)]}
  (char (+ (mod (- num 1) 26) 65)))

(defn letter-to-number
  "Return the integer that corresponds to the given character where \\A (or \\a)
  is 1 and \\Z (or \\z) is 26."
  [chr]
  {:pre [(valid-char? chr)]}
  (- (int (Character/toUpperCase chr)) 64))

(defn move-card-down
  "With two arguments, moves the given card one space closer to the bottom of
the deck. With three arguments, moves the given card the given number of spaces
toward the bottom of the deck. If card moves past bottom of deck, it cycles back
to the top of the deck."
  ([deck card]
     {:pre [(valid-deck? deck) (valid-card? card)]}
     (let [[top [_ next-card & bottom]] (split-with #(not= % card) deck)]
       (if (not next-card)
         (list* (first top) card (rest top))
         (concat top [next-card card] bottom))))
  ([deck card spaces]
     {:pre [(valid-deck? deck) (valid-card? card) (integer? spaces)]}
     (if (<= spaces 0)
       deck
       (recur (move-card-down deck card) card (dec spaces)))))

(defn split-at-jokers
  "Returns a three-element vector whose first element is the chunk of
  cards before the first joker, whose second element is the chunk of
  cards starting with the first joker followed by all the cards after
  the first joker but before the second joker, and finally whose third
  element is the chunk of cards starting with the second joker
  followed by the rest of the cards in the deck."
  [deck]
  {:pre [(valid-deck? deck)]}
  (let [[top [first-joker & therest]] (split-with #(not (joker? %)) deck)
        [middle bottom] (split-with #(not (joker? %)) therest)
        middle (cons first-joker middle)]
    [top middle bottom]))

(defn triple-cut
  "Performs a triple cut around the two Jokers: the chunk of cards
  above the first Joker swaps places with the chunk of cards below the
  second Joker."
  [deck]
  {:pre [(valid-deck? deck)]}
  (let [[top middle [second-joker & bottom]] (split-at-jokers deck)
        middle (concat middle (list second-joker))]
    (concat bottom middle top)))

(defn cut-preserve-bottom
  "Returns a new deck which is the result of performing a cut at position
and leaving the bottom card in place."
  [deck position]
  {:pre [(valid-deck? deck) (<= position 54)]}
  (let [[top therest] (split-at position deck)
        [middle bottom] (split-at (dec (count therest)) therest)]
    (concat middle top bottom)))

(defn count-cut
  "Returns a new deck which is the result of performing a cut at the value
of the last card (a joker is 53) but the last card stays in place."
  [deck]
  {:pre [(valid-deck? deck)]}
  (let [bottomcard (if (joker? (last deck)) 53 (last deck))]
    (cut-preserve-bottom deck bottomcard)))

(defn solitaire
  "Performs one iteration of the solitaire algorithm on deck and returns
the modified deck. deck is a sequence of integers from 1 to 54 in any order."
  [deck]
  {:pre [(valid-deck? deck)]}
  (-> deck
      (move-card-down jokerA)
      (move-card-down jokerB 2)
      triple-cut
      count-cut))

(defn key-deck
  "Uses the given passphrase to key deck using the solitaire algorithm."
  [deck passphrase]
  {:pre [(valid-deck? deck)
         (valid-string? passphrase)]}
  (if (empty? passphrase)
    deck
    (let [char-val (letter-to-number (first passphrase))
          new-deck (-> deck
                       solitaire
                       (cut-preserve-bottom char-val))]
      (key-deck new-deck (next passphrase)))))

(defn generate-key
  "Takes a deck and returns a vector containing the next valid key and the deck
used to generate it. The deck is returned because the solitaire algorithm is
performed on the passed in deck when the generated key is a joker."
  [deck]
  {:pre [(valid-deck? deck)]}
  (let [topcard (first deck)
        topcard (if (joker? topcard) 53 topcard)
        [_ bottom] (split-at topcard deck)
        key (first bottom)]
    (if (joker? key)
      (generate-key (solitaire deck))
      [key deck])))

(defn solitaire-keystream
  "An infinite sequence of keys generated by performing the solitaire
algorithm on the given deck repeatedly producing a key each time."
  [deck]
  {:pre [(valid-deck? deck)]}
  (let [[key newdeck] (generate-key (solitaire deck))]
    (lazy-seq
     (cons key (solitaire-keystream newdeck)))))

(defn encrypt
  "Encrypts the message text using the given deck. Returns the cypher text as
string."
  [message deck]
  {:pre [(valid-deck? deck)
         (valid-string? message)]}
  (let [key-stream (solitaire-keystream deck)
        msg (pad-to-mod-5-with-x message)
        message-vals (map letter-to-number msg)
        encoded-vals (->> (map #(+ %1 %2) key-stream message-vals)
                          (map #(if (<= % 26) % (recur (- % 26)))))]
    (apply str (map number-to-letter encoded-vals))))

(defn decrypt
  "Decrypts the given encoded message using the given keyed deck. Returns the
  decoded message as a string."
  [encrypted-message deck]
  {:pre [(valid-deck? deck)
         (= 0 (mod (count encrypted-message) 5))]}
  (let [key-stream (solitaire-keystream deck)
        message-vals (map letter-to-number encrypted-message)
        decoded-vals (->> (map #(- %1 %2) message-vals key-stream)
                          (map #(if (<= % 0) (recur (+ % 26)) %)))]
    (apply str (map number-to-letter decoded-vals))))
