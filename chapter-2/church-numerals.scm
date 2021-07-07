(define zero
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x)
                (f ((n f) x)))))

(define (sum a b)
  (lambda (f) (lambda (x)
                ((a f) ((b f) x)))))

(define (mul a b)
  (lambda (f) (lambda (x)
                ((a (b f))
                 x))))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (church2int n)
  ((n 1+) 0))
