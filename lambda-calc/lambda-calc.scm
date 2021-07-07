(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (<= n 1)
      f
      (compose f (repeated f (1- n)))))

;; usage: (curried (x y) (* x y))
(define-syntax curried
  (syntax-rules ()
    ((curried (arg arg* ...) exp ...)
     (lambda (arg) (curried (arg* ...) exp ...)))
    ((curried () exp ...)
     (begin exp ...))))

;; shorthand for (define x (curried (a b c) (* a b c)))
;; is (def-curried (x a b c) (* a b c))
(define-syntax-rule (define-curried (name arg ...) exp ...)
  (define name (curried (arg ...) exp ...)))

(define-syntax run-curried
  (syntax-rules ()
    ((run-curried func arg arg* ...)
     (run-curried (func arg) arg* ...))
    ((run-curried result)
     result)))

(define-syntax \
  (identifier-syntax curried))
(define-syntax >>
  (identifier-syntax run-curried))
(define-syntax def\
  (identifier-syntax define-curried))

;;;
;;; Numbers
;;;

(def\ (zero f x)
  x)

(def\ (succ n)
  (\ (f x) (f (>> n f x))))

(def\ (pred n)
  (\ (f x) (>> n (\ (g h) (h (g f))) (\ (u) x) (\ (u) u))))

(def\ (sum a b)
  ((b succ) a))

(def\ (sum2 a b)
  (\ (f x) ((a f) (>> b f x))))

(def\ (minus a b)
  ((b pred) a))

(def\ (mul a b)
  (\ (f x)
   ((a (b f)) x)))

(def\ (exp m n)
  (n m))

(def\ (one f x)
  (f x))

(def\ (two f x)
  (f (f x)))

(define (i< c)
  (>> c 1+ 0))
(define (c< i)
  ((repeated succ i) zero))

;;;
;;; Booleans
;;;

;; if statements require passing arguments as lambdas
;; if we don't want to evaluate both operands (we want normal order,
;; lisp operates in applicative order)
(def\ (true a b) a)
(def\ (false a b) b)
;; (define cif true)

(def\ (cand a b)
  (>> a b a))
(def\ (cor a b)
  (>> a a b))

(def\ (cnot x)
  (>> x false true))

;;;
;;; Predicates
;;;

(def\ (neq0 n)
  (>> n (\ (x) true) false))
(def\ (eq0 n)
  (>> n (\ (x) false) true))

(def\ (less/eq a b)
  (eq0 (>> minus a b)))
(def\ (greater/eq a b)
  (eq0 (>> minus b a)))
(def\ (less a b)
  (cnot (>> greater/eq a b)))
(def\ (greater a b)
  (cnot (>> less/eq a b)))

(define (cb< b)
  (if b true false))
(define (b< cb)
  (>> cb #t #f))

;;;
;;; Problems
;;;

(def\ (fib n)
  ((>> (>> less n two)
       (lambda () one)
       (lambda () (>> sum (fib (pred n)) (fib (pred (pred n)))))) ))
