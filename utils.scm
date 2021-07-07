(define-module (utils)
  #:export (runtime runtime-second func-time))

(define (runtime) (tms:clock (times)))  ; 1/1000000000 of a second (nanosecond)
(define (runtime-second) (/ (runtime) 1000000000))
(define* (func-time func #:optional (div 1))
  (/ 
   (let ((start (runtime)))
     (func)
     (- (runtime) start))
   (* div 1000000000)))
