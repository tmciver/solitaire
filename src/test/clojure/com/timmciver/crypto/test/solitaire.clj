(ns com.timmciver.crypto.test.solitaire
  (:use clojure.test
        com.timmciver.crypto.solitaire))

(deftest test-move-card-down
  (let [deck [5 7 2 4 3 9]]
    (is (= (move-card-down deck 4 1) [5 7 2 3 4 9]))
    (is (= (move-card-down deck 4 4) [5 7 4 2 3 9]))))

(deftest test-triple-cut
  (let [deck [1 2 3 53 5 6 7 54 8 9]]
    (is (= (triple-cut deck) [8 9 53 5 6 7 54 1 2 3]))))