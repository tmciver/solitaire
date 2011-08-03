(ns com.timmciver.crypto.test.solitaire
  (:use clojure.test
        com.timmciver.crypto.solitaire))

(deftest test-move-card-down
  (let [orig-deck [5 7 2 4 3 9]
        mod-deck (move-card-down orig-deck 4 1)]
    (is (= mod-deck [5 7 2 3 4 9]))))