(in-ns 'nefarious.number-theory.core)

; quadratic-solutions : Real Real Real -> (Listof Real)
(defn quadratic-solutions
  "Returns list of solutions to a x^2 + b x + c = 0"
  [a b c]
  (let [d (- (* b b) (* 4 a c))]
    (cond
      (< d 0) ()
      (= d 0) (list (/ b (* -2 a)))
      :else
       (let [sqrt-d (m/sqrt d)]
         (list (/ (- (- b) sqrt-d) (* 2 a))
               (/ (+ (- b) sqrt-d) (* 2 a)))))))

; quadratic-integer-solutions : Real Real Real -> (Listof Integer)
(defn quadratic-integer-solutions [a b c]
  (filter integer? (quadratic-solutions a b c)))

; quadratic-natural-solutions : Real Real Real -> (Listof Natural)
(defn quadratic-natural-solutions [a b c]
  (filter (fn [x] (or (pos? x) (zero? x))) (quadratic-integer-solutions a b c)))

