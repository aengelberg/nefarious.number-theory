(in-ns 'nefarious.number-theory.core)

; generator : Integer Integer Natural Natural Natural -> Integer
(defn generator 
  [a b p q count]
  (cond 
    (zero? count) b
    (even? count)
    (generator a b
               (+' (*' p p) (*' q q))
               (+' (*' 2 p q) (*' q q))
               (quot count 2))
    :else
    (generator (+' (*' b q) (*' a q) (*' a p))
               (+' (*' b p) (*' a q))
               p
               q
               (- count 1))))

; make-fibonacci (Integer Integer -> (Integer -> Integer)))
(defn make-fibonacci [a b]
  (fn [n]
    {:pre [(>= n 0) (integer? n)]}
    (generator b a 0 1 n)))

(def fibonacci (make-fibonacci 0 1))

;modular-generator : Integer Integer Natural Natural Natural Positive-Integer -> Integer
(defn modular-generator 
  [a b p q count modulus]
  (cond 
    (zero? count) (mod b modulus)
    (even? count)
    (recur
      a b
      (mod (+' (*' p p) (*' q q)) modulus)
      (mod (+' (*' 2 p q) (*' q q)) modulus)
      (quot count 2)
      modulus)
    :else
    (recur
      (mod (+' (*' b q) (*' a q) (*' a p)) modulus)
      (mod (+' (*' b p) (*' a q)) modulus)
      p
      q
      (dec count)
      modulus)))

; make-modular-fibonacci (Integer Integer -> (Integer Integer -> Integer)))
(defn make-modular-fibonacci [a b]
  (fn [n modulus]
    {:pre [(>= n 0) (> modulus 0) (integer? n) (integer? modulus)]}
    (modular-generator b a 0 1 n modulus)))

(def modular-fibonacci (make-modular-fibonacci 0 1))
