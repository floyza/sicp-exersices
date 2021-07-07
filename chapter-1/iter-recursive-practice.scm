;;; Exercise 1.11
(define (f n)
  (if (< n 3)
      n
      (+ (f (1- n))
         (* 2
            (f (- n 2)))
         (* 3
            (f (- n 3))))))
(f 10)
(define (f-helper a b c i n)
  (if (> i n)
      a
      (f-helper
       (if (< i 3)
           i
           (+ a (* 2 b) (* 3 c)))
       a b (1+ i) n)))
(define (f-iter n)
  (f-helper 0 0 0 0 n))
(f-iter 10)
