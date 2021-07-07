(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((result (f guess)))
      ;; (display result)
      ;; (newline)
      (if (close-enough? guess result)
          result
          (try result))))
  (try first-guess))

(define (average x y) (/ (+ x y) 2))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;; Exercise 1.35
(fixed-point (lambda (x) (1+ (/ 1 x)))
             1.0)

;; Exercise 1.37
;; (define (cont-frac n d k)
;;   "Compute the `k'-term continued fraction with procedures `n' and `d'."
;;   (/ (n k)
;;      (+ (d k)
;;         (cont-frac n d))))
(define (cont-frac n d k)
  (/ (n (- 2 k))
     (+ (d (- 2 k))
        (/ (n (1- k))
           (+ (d (1- k))
              (/ (n k)
                 (d k)))))))
