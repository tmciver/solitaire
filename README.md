# Solitaire Encryption Algorithm

This project is a Clojure implementation of [Bruce Schneier's Solitaire
encryption algorithm](http://www.schneier.com/solitaire.html).

## Usage

Encrypting a message with the solitaire encryption algorithm requires a keyed
deck. The particular order of the cards in a keyed deck does not matter, but it
must be known to both the sender and receiver of the encrypted message. The deck
used to encrypt a message includes both jokers and so has 54 cards. This
implementation represents a deck as a collection of the integers 1 through
54. To key a deck, one starts with a deck in a known order and then uses the
solitaire alogorithm with a passphrase to re-order the deck. For example, we can
start with an *ordered deck* and key it with a passphrase as follows:

    (def keyed-deck (key-deck ordered-deck "password"))

This gives the following keyed deck:

    (52 5 42 8 9 10 11 12 13 14 15 16 17 1 20 21 22 23 19 4 53 6 7 24 41 43 44 3
    54 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 2 18 45 46 47 48 49 50 51 25)

Once we have a keyed deck we can use it to encrypt a message:

    (def encrypted-message (encrypt "DONOTUSEPC" keyed-deck))

and the encrypted message is:

    "PTLOOJPWHZ"

Note that the algorithm can deal only with alphabetic characters and so spaces
and puncuation are not allowed in either the passphrase or the message to be encrypted.

To decrypt the message:

    (decrypt encrypted-message keyed-deck)

Which gives the original message back.

It's tradition to group a message to be encrypted into groups of five characters
and to pad the message with X's if its length is not a multiple of five. If you
encrypt and then decrypt such a message you'll see these X's in the output:

    (def encrypted-message (encrypt "SECRETMESSAGE" keyed-deck))
    (def decrypted-message (decrypt encrypted-message keyed-deck))

The decrypted message is:

    "SECRETMESSAGEXX"

Of course to get back only the original message it's a simple matter of taking
the correct number of characters:

    (take (count original-message) decrypted-message)