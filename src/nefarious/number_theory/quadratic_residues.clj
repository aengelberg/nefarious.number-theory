(in-ns 'nefarious.number-theory.core
       (:use nefarious.number-theory.divisibility
             nefarious.number-theory.modular_arithmetic
             nefarious.number-theory.number_theory))

; DEFINITION (Quadratic residue)
;   a in Un is a quadratic residue,
;   if there exists an s such that a=s^2 (mod n)
;   The number s is called a squre root of a modulo n.

; p is prime
; quadratic-character : Integer Integer -> (U -1 0 1)
(defn quadratic-character 
  [a p]
  (cond 
    (a < 0) (raise-argument-error 'quadratic-character "Natural" 0 a p) ; what is "raise-argument-error" in clojure terms?
    (p <= 0) (raise-argument-error 'quadratic-character "Positive-Integer" 1 a p)
    :else (let [l  (modular-expt a (quotient (- p 1) 2) p)] ; modular-expt???
            (cond 
              (or (= l 0) (= l 1))  l ; eqv???
              :else  -1))))

; quadratic-residue? : Integer Integer -> Boolean
(defn quadratic-residue?
  [a n]
  (cond
    (< a 0) (raise-argument-error 'quadratic-residue? "Natural" 0 a n)
    (<= n 0) (raise-argument-error 'quadratic-residue? "Positive-Interger" 1 a n)
    :else (let* [ps (prime-divisors n)
                 odd-ps (if (= (first ps) 2)
                          (rest ps)
                          ps)]
            (and (andmap (λ: [p : Natural]
                           (= (quadratic-character a p) 1))
                         odd-ps)
                 (cond
                   (divides? 8 n) (= (modulo a 8) 1)
                   (divides? 4 n) (= (modulo a 4) 1)
                   :else true)))))

