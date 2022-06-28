#lang racket
(provide (all-defined-out))

(define (check_bst xs)
  (define (check xs max min)
      (if (null? xs)
          #t
          (let ([cur (car xs)]
                [left (car (cdr xs))]
                [right (car (cdr (cdr xs)))])
            (if (or (<= cur min) (<= max cur))
                #f
                (and (check left cur min) (check right max cur))))))
  (check xs +inf.0 -inf.0))

(define (apply f xs)
  (if (null? xs)
      `()
      (let ([cur (car xs)]
            [left (car (cdr xs))]
            [right (car (cdr (cdr xs)))])
        (cons (f cur) (cons (apply f left) (cons (apply f right) empty))))))

(define (equals xs ys)
  (define (check x l)
    (if (null? l)
        #f
        (let ([cur (car l)]
              [left (car (cdr l))]
              [right (car (cdr (cdr l)))])
          (if (= x cur)
              #t
              (if (< x cur)
                  (check x left)
                  (check x right))))))
  (define (length xs)
    (if (null? xs)
        0
        (let ([left (car (cdr xs))]
              [right (car (cdr (cdr xs)))])
          (+ 1 (length left) (length right)))))
  (define (traversal xs ys)
    (if (null? xs)
          #t
          (let ([cur (car xs)]
                [left (car (cdr xs))]
                [right (car (cdr (cdr xs)))])
            (if (check cur ys)
                (and (traversal left ys) (traversal right ys))
                #f))))
  (if (>= (length xs) (length ys))
      (traversal xs ys)
      (traversal ys xs)))
