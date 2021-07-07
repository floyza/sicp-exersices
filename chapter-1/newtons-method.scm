(load "../chapter-1/fixed-point.scm")

(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (square x)
  (* x x))
(define (cube x)
  (* x x x))

(define* (average #:rest args)
  (/ (apply + args) (length args)))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (average-damp g)
  (lambda (x) (average x (g x))))

(define (sqrt1 x)
  (fixed-point-of-transform
   (lambda (y) (/ x y)) average-damp 1.0))

(define (sqrt2 x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x)) newton-transform 1.0))

;; Exercise 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;; Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

;; Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;; Exercise 1.43
(define (repeated f n)
  (if (<= n 1)
      f
      (compose f (repeated f (1- n)))))

;; Exercise 1.44
(define (smooth f dx)
  (lambda (x) (average (f (- x dx))
                       (f x)
                       (f (+ x dx)))))
(define (nsmooth f dx n)
  (repeated (smooth f dx) n))
