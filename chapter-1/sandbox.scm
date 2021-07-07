(add-to-load-path "..")
(use-modules (utils))

(define (sum-integers a b)
  "Sums integers in the range [a..b]"
  (if (> a b)
      0
      (+ a (sum-integers (1+ a) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0
            (* a
               (+ a 2)))
         (pi-sum (+ a 4) b))))

(define (chance-not-in-tries chance tries)
  (if (> tries 0)
      (* (- 1 chance) (chance-not-in-tries chance (1- tries)))
      1))

(define (chance-in-tries chance tries)
  (- 1 (chance-not-in-tries chance tries)))
