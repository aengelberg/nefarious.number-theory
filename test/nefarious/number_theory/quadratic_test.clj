(ns nefarious.number-theory.quadratic-test
    (:use clojure.test
          nefarious.number-theory.core))

(deftest quadratic-test
  (is (or (= (quadratic-solutions  1 1 -6) '(-3 2)) (= (quadratic-solutions  1 1 -6) '(2 -3))))
  (is (= (quadratic-solutions 1 6 9) '(-3)))
  (is (= (quadratic-solutions 2 1 1)) '())
  (is (= (quadratic-integer-solutions 2 5 -3)) '(-3))
  (is (= (quadratic-natural-solutions  1 1 -6) '(2)))
  (is (= (quadratic-natural-solutions 2 5 -3) '())))