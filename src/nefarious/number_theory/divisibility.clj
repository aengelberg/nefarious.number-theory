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

(defn- bezout-start
  [a b]
  (if (> a b)
    (bezout-loop a b 1 0 0 1)
    (bezout-loop b a 0 1 1 0)))

(defn- bezout-binary
  [a b]
  (cond
    (zero? a) (if (pos? b)
                [1 1]
                [1 -1])
    (zero? b) (if (pos? a)
                [1 1]
                [-1 1])
    :else (case [(pos? a) (pos? b)]
            [true true] (bezout-start a b)
            [false false] (mapv - (bezout-start (- a) (- b)))
            [false true] (let [k (+ (quot (- a) b) 1)
                               [u v] (bezout-start (+ a (* k b)) b)]
                           [u (+ (* u k) v)])
            [true false] (let [k (+ (quot (- b) a) 1)
                               [u v] (bezout-start a (+ (* k a) b))]
                           [(+ u (* k v)) v]))))

(defn bezout
  "Given a list (a b c ...), returns a list (u v w ...) such that a*u + b*v + c*w + ... = gcd(a, b, c, ...)."
  [a & more]
  (cond
    (empty? more) (if (neg? a)
                    (list -1)
                    (list 1))
    (empty? (rest more)) (seq (bezout-binary a (first more)))
    :else (let [uvs (apply bezout more)
                [s t] (bezout-binary (apply gcd-multi more) a)]
            (cons t (map #(* s %) uvs)))))