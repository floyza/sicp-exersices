(define (mult a b)
  (if (= b 0)
      0
      (+ a (mult a (1- b)))))

(define (double x)
  (* x 2))
(define (half x)
  (/ x 2))
(define (fast-mult-helper a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult-helper a (half b))))
        (else (+ a (fast-mult-helper a (1- b))))))
(define (fast-mult a b)
  (if (< b 0)
      (fast-mult-helper (- a) (- b))
      (fast-mult-helper a b)))
(fast-mult 5 6)
