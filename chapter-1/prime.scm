(define (runtime) (tms:clock (times)))

(define (square x) (* x x))
(define (divides? a b) (= (remainder b a) 0))


(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))


;; use fast-expt method (succesive squaring)
;; base^exp mod m
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (1+ (random (1- n)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (1- times)))
        (else #f)))


(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (search-for-primes lo hi)
  (cond ((> lo hi) 0)
        ((even? lo)
         (search-for-primes (1+ lo) hi))
        (else
         (timed-prime-test lo)
         (search-for-primes (+ 2 lo) hi))))

(define (check-err-prime lo hi sum)
  (let ((times 10))
    (cond ((> lo hi) sum)
          ((even? lo) (check-err-prime (1+ lo) hi sum))
          (else
           (check-err-prime (+ 2 lo) hi (let ((base (prime? lo))
                                              (fast (fast-prime? lo times)))
                                          (if (not (eq? base fast))
                                              (begin
                                                (display lo)
                                                (newline)
                                                (1+ sum))
                                              sum)))))))
;; (check-err-prime 2 100000 0)

(search-for-primes 1000000000000 1000000000100)
;; 
;; takes:     20000000
;; *10 takes: 60000000
;; (sqrt 10) = 3.1622776601683795
;; lowest after:
;; 1000: 1009, 1013, 1019
;; 10,000: 10007, 10009, 10037
;; 100,000: 100003, 100019, 100043
;; 1,000,000: 1000003, 1000033, 1000037
