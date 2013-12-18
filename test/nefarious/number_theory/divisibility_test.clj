(ns nefarious.number-theory.divisibility-test
  (:use nefarious.number-theory.core
        clojure.math.numeric-tower
        clojure.test))

(deftest divides?-test
  (doseq [i (range -3 4)
          j (range -3 4)]
    (if (= i 0)
      (is (not (divides? i j)))
      (is (= (divides? i j)
             (integer? (/ j i)))))))

(deftest coprime?-test
  (doseq [i (range -3 4)
          :when (not= i 0)]
    (is (= (coprime? i)
           (= (abs i) 1))))
  (doseq [i (range -3 4)
          j (range -3 4)
          :when (not= i j 0)]
    (is (= (coprime? i j)
           (= (gcd i j) 1))))
  (doseq [i (range -3 4)
          j (range -3 4)
          k (range -3 4)
          :when (not= i j k 0)]
    (is (= (coprime? i j k)
           (= (reduce gcd [i j k]) 1)))))

(deftest pairwise-coprime?-test
  (are [x] x
       (pairwise-coprime? 1 3 5)
       (pairwise-coprime? 1 3 7)
       (pairwise-coprime? 2 3 5 7))
  (are [x] (not x)
       (pairwise-coprime? 1 3 6)
       (pairwise-coprime? 2 6 3)
       (pairwise-coprime? 0 2 3)))

(deftest bezout-test
  (doseq [len (range 1 5)]
    (dotimes [i 30]
      (let [l (repeatedly len #(- (rand-int 10) 5))
            ;_ (println l)
            ]
        (when (apply not= 0 l)
          (let [b (apply bezout l)
                ;_ (println b)
                ]
            (is (= (apply + (map * l b))
                   (abs (reduce gcd l))))))))))