;;; Exercise 1.12

(define (pascal row col)
  (if (or (= col 0) (= col row))
      1
      (+ (pascal (1- row) (1- col))
         (pascal (1- row) col))))
(pascal 4 1)
