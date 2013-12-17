(in-ns 'nefarious.number-theory.core)
(use 'clojure.math.numeric-tower)

(defn- gcd-multi
  ([x]
    (abs x))
  ([x y]
    (gcd x y))
  ([x y & more]
    (reduce gcd (list* x y more))))

(defn divides?
  "Given integers a and b, does a divide b? I.e. is b/a an integer?
Formally, a divides b <=> there exists a unique integer k such that a*k = b."
  [a b]
  (cond
    (zero? a) false
    :else (zero? (mod b a))))

(defn coprime?
  "Given integers a, b, etc. are they coprime? I.e. gcd(a, b, ...) = 1?"
  ([a]
    (= (abs a) 1))   ; gcd(a) = 1 <=> abs(a) = 1
  ([a b]
    (= (gcd a b) 1))
  ([a b & more]
    (= (reduce gcd (list* a b more)) 1)))

(defn pairwise-coprime?
  "Is every pair of integers coprime? E.g. the numbers 2, 6, and 15 are coprime,
but not pairwise coprime, because 2 and 6 share a divisor.
Runs in O(N^2) time, in terms of calls to gcd."
  ([a & more]
    (or (empty? more)
        (and (every? #(coprime? a %) more)
             (recur (first more) (next more))))))

(defn- bezout-loop
  [a b ua va ub vb]
  (let [r (rem a b)
        q (quot a b)]
    (if (= r 0)
      [ub vb]
      (recur b r ub vb (- ua (* q ub)) (- va (* q vb))))))

(defn- bezout-binary
  [a b]
  (cond
    (zero? a) (list 0 1)
    (zero? b) (list 1 0)
    :else (let [[a b ua va ub vb] (if (> a b)
                                    [a b 1 0 0 1]
                                    [b a 0 1 1 0])]
            (loop [a a
                   b b
                   ua ua
                   va va
                   ub ub
                   vb vb]
              (let [r (mod a b)
                    q (quot a b)]
                (if (= r 0)
                  (list ub vb)
                  (recur b r ub vb (- ua (* q ub)) (- va (* q vb)))))))))

(defn bezout
  "Given a list (a b c ...), returns a list (u v w ...) such that gcd(a, b, c, ...) = au + bv + cw + ..."
  [a & more]
  (cond
    (empty? more) (list a)
    (empty? (rest more)) (bezout-binary a (first more))
    :else (let [uvs (apply bezout more)
                [s t] (bezout-binary (apply gcd-multi more) a)]
            (cons t (map #(* s %) uvs)))))