(define e (/ (1+ (sqrt 5)) 2))
(define (fib n)
  (if (< n 3)
      1
      (+ (fib (1- n)) (fib (- n 2)))))
(define (fake-fib n)
  (/ (expt e n) (sqrt 5)))
(fib 5)
(fake-fib 5)
