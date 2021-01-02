#lang racket
;;;;;;;;;;;;;;;;;;;;;;; TASK 4 ;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
; i:    1  2  3  4  5  6  7  8  9  10 11 ...
; f(i): 1  1  2  3  5  8  13 21 34 55 ...

(define fibonacci
  (lambda (n)
    (cond ((eq? n 1) 1)
          ((eq? n 2) 1)
          (else (fibonacci-c n 0 1 (lambda (next) (+ 1 next)))))))

(define fibonacci-c
  (lambda (n n1 n2 cont)
    (if (= n 3)
        (cont 1)
        (fibonacci-c (- n 1) n2 (+ n1 n2) (lambda (next) (cont (+ next (+ n1 n2))))))))

;; Tests
(display (fibonacci 4)) ; should output 3
(display  "\n")
(display (fibonacci 7)) ; should output 13
(display  "\n")
(display (fibonacci 8)) ; should output 21
(display  "\n")