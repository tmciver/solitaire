(ns com.timmciver.crypto.test.solitaire
  (:use clojure.test
        com.timmciver.crypto.solitaire))

(deftest test-number-to-letter
  (is (= (number-to-letter [1 2 3]) [\A \B \C]))
  (is (= (number-to-letter [26 27 28]) [\Z \A \B])))

(deftest test-letter-to-number
  (is (= (letter-to-number "Hello World") [8 5 12 12 15 23 15 18 12 4])))

(deftest test-move-card-down
  (let [deck [19 35 39 26 16 45 34 43 20 22 18 33 44 2 10 54 25 4 49 21 29 15 14 7 42 5 23 31 48 9 1 6 51 27 38 41 11 50 24 17 8 53 36 13 3 37 12 30 32 40 52 46 47 28]]
    (is (= (move-card-down deck 4 3) [19 35 39 26 16 45 34 43 20 22 18 33 44 2 10 54 25 49 21 29 4 15 14 7 42 5 23 31 48 9 1 6 51 27 38 41 11 50 24 17 8 53 36 13 3 37 12 30 32 40 52 46 47 28]))
    (is (= (move-card-down deck 46 5) [19 35 39 46 26 16 45 34 43 20 22 18 33 44 2 10 54 25 4 49 21 29 15 14 7 42 5 23 31 48 9 1 6 51 27 38 41 11 50 24 17 8 53 36 13 3 37 12 30 32 40 52 47 28])))
  (is (= (move-card-down ordered-deck jokerA 1)
         [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 54 53])))

(deftest test-triple-cut
  (let [deck [20 26 7 4 34 25 49 38 17 31 3 43 51 5 50 53 9 47 11 52 15 24 6 14 46 10 28 39 44 16 48 12 32 33 45 36 40 19 2 42 8 27 23 18 54 30 13 41 35 29 21 37 1 22]]
    (is (= (triple-cut deck) [30 13 41 35 29 21 37 1 22 53 9 47 11 52 15 24 6 14 46 10 28 39 44 16 48 12 32 33 45 36 40 19 2 42 8 27 23 18 54 20 26 7 4 34 25 49 38 17 31 3 43 51 5 50])))
  (let [deck [20 26 7 4 34 25 49 38 17 31 3 43 51 5 50 53 9 47 11 52 15 24 6 14 46 10 28 39 44 16 48 12 32 33 45 36 40 19 2 42 8 27 23 18 30 13 41 35 29 21 37 1 22 54]]
    (is (= (triple-cut deck) [53 9 47 11 52 15 24 6 14 46 10 28 39 44 16 48 12 32 33 45 36 40 19 2 42 8 27 23 18 30 13 41 35 29 21 37 1 22 54 20 26 7 4 34 25 49 38 17 31 3 43 51 5 50]))))

(deftest test-cut-preserve-bottom
  (let [deck [20 26 7 4 34 25 49 38 17 31 3 43 51 5 50 53 9 47 11 52 15 24 6 14 46 10 28 39 44 16 48 12 32 33 45 36 40 19 2 42 8 27 23 18 54 30 13 41 35 29 21 37 1 22]]
    (is (= (cut-preserve-bottom deck 19) [52 15 24 6 14 46 10 28 39 44 16 48 12 32 33 45 36 40 19 2 42 8 27 23 18 54 30 13 41 35 29 21 37 1 20 26 7 4 34 25 49 38 17 31 3 43 51 5 50 53 9 47 11 22]))))

(deftest test-count-cut
  (let [deck [20 26 7 4 34 25 49 38 17 31 3 43 51 5 50 53 9 47 11 52 15 24 6 14 46 10 28 39 44 16 48 12 32 33 45 36 40 19 2 42 8 27 23 18 54 30 13 41 35 29 21 37 1 22]]
    (is (= (count-cut deck) [6 14 46 10 28 39 44 16 48 12 32 33 45 36 40 19 2 42 8 27 23 18 54 30 13 41 35 29 21 37 1 20 26 7 4 34 25 49 38 17 31 3 43 51 5 50 53 9 47 11 52 15 24 22])))
  (let [deck [20 26 7 4 34 25 49 38 17 31 3 43 51 5 50 53 9 47 11 52 15 24 6 14 46 10 28 39 44 16 48 12 32 33 45 36 40 19 2 42 8 27 23 18 30 13 41 35 29 21 37 1 22 54]]
    (is (= (count-cut deck) [20 26 7 4 34 25 49 38 17 31 3 43 51 5 50 53 9 47 11 52 15 24 6 14 46 10 28 39 44 16 48 12 32 33 45 36 40 19 2 42 8 27 23 18 30 13 41 35 29 21 37 1 22 54]))))

(deftest test-solitaire
  (let [deck [19 35 39 26 16 45 34 43 20 22 18 33 44 2 10 54 25 4 49 21 29 15 14 7 42 5 23 31 48 9 1 6 51 27 38 41 11 50 24 17 8 53 36 13 3 37 12 30 32 40 52 46 47 28]]
    (is (= (solitaire deck) [30 32 40 52 46 47 28 54 49 21 29 15 14 7 42 5 23 31 48 9 1 6 51 27 38 41 11 50 24 17 8 36 53 19 35 39 26 16 45 34 43 20 22 18 33 44 2 10 25 13 3 37 12 4]))))

(deftest test-solitaire-keystream
  (is (= (take 10 (solitaire-keystream ordered-deck))
         [4 49 10 24 8 51 44 6 4 33])))

(deftest test-testvectors
  (are [key plaintext cyphertext]
       (= cyphertext (encode plaintext (key-deck ordered-deck key)))
       nil "AAAAAAAAAAAAAAA" "EXKYIZSGEHUNTIQ"
       "f" "AAAAAAAAAAAAAAA" "XYIUQBMHKKJBEGY"
       "fo" "AAAAAAAAAAAAAAA" "TUJYMBERLGXNDIW"
       "foo" "AAAAAAAAAAAAAAA" "ITHZUJIWGRFARMW"
       "a" "AAAAAAAAAAAAAAA" "XODALGSCULIQNSC"
       "aa" "AAAAAAAAAAAAAAA" "OHGWMXXCAIMCIQP"
       "aaa" "AAAAAAAAAAAAAAA" "DCSQYHBQZNGDRUT"
       "b" "AAAAAAAAAAAAAAA" "XQEEMOITLZVDSQS"
       "bc" "AAAAAAAAAAAAAAA" "QNGRKQIHCLGWSCE"
       "bcd" "AAAAAAAAAAAAAAA" "FMUBYBMAXHNQXCJ"
       "cryptonomicon" "AAAAAAAAAAAAAAAAAAAAAAAAA" "SUGSRSXSWQRMXOHIPBFPXARYQ"
       "cryptonomicon" "SOLITAIRE" "KIRAKSFJAN"))
