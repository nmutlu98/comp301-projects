(module project1 mzscheme
  
  ;;;;;;;;;;;; Comp 301 Project 1 ;;;;;;;;;;;;;;;;
  ;;; Add group members below

  ;;; Necla Mutlu, nmutlu17, 0064032
  ;;; Firat Tamur, ftamur16, 0060534
  
  ;;; save your file in the format: p1_0064032_nmutlu17_0060534_ftamur16.rkt
  
  ;;;;;;;;;;;;;;;;;;; PROBLEMS ;;;;;;;;;;;;;;;;;;;;
  ;; PROJECT 1 Part A | Write your answer below here as a comment
  ; 
  ; First Grammar Definition:
  ; Define even numbers as list of 'even and 'two.

  ; evens ::= ('even) | (evens . 'two)
  
  ; [0]   = ('even)
  ; [n+1] = ([n] . 'two)

  ; 
  ; Second Grammar Definition:
  ; Define even numbers as a list containing the elements of the representation in the hexadecimal format
  ;
  ; [n] = () if n = 0
  ; [n] = (a . [q]) if n = 16*q + r,
  ;                    for 0<= r <10 a = r,
  ;                    for r = 10 a = A,
  ;                    for r = 11 a = B,
  ;                    for r = 12 a = C,
  ;                    for r = 13 a = D,
  ;                    for r = 14 a = E,
  ;                    for r = 15 a = F.
  ; 
  ;
  ;
  ;; PROJECT 1 Part B
  ;; First Representation | We added a suffix so that both first and second representations can be tested at once.

  (define create-a
    (lambda (n)
      (if (= n 0)
          (list 'even)
          (append (create-a (- n 2)) '(two)))
      )
    )

  (define is-zero-a?
    (lambda (n) (and (eqv? (car n) 'even)) (null? (cdr n)))
    )

  (define successor-a
    (lambda (n)
      (if (null? n)
          (create-a n)
          (append n '(two)))
      )
    )

  ;; Second Representation | We added a -b suffix so that both Unary and BigNum can be tested at once.

  ;; number -> symbol | number
  ;; returns the equivalent character of a number in hexadecimal system if number is greater than 9
  ;; returns the number itself if it is smaller or equal to 9
  (define find-char-equivalent
    (lambda (n)
      (cond ((eq? n 10) 'A)
            ((eq? n 11) 'B)
            ((eq? n 12) 'C)
            ((eq? n 13) 'D)
            ((eq? n 14) 'E)
            ((eq? n 15) 'F)
            (else n)
            )))
  
  ;; symbol | number -> number
  ;; returns the associated number of n in hexadecimal system if n is a symbol, otherwise returns the n itself
  (define find-number-equivalent
    (lambda (n)
      (cond ((eq? n 'A) 10)
            ((eq? n 'B) 11)
            ((eq? n 'C) 12)
            ((eq? n 'D) 13)
            ((eq? n 'E) 14)
            ((eq? n 'F) 15)
            (else n)
            )))
  
  (define create-b
    (lambda (n)
      (cond ((eq? n 0) '())
            ((< n 16) (list (find-char-equivalent n)))
            (else (cons (find-char-equivalent (modulo n 16)) (create-b (truncate (/ n 16))))))))

  (define is-zero-b?
    (lambda (n) (null? n))
    )

  ;; even-number -> even-number
  ;; returns the even-number incremented by 1
  (define increment-b
    (lambda (n)
      (cond ((null? n) (create-b 1))
            ((< (find-number-equivalent (car n)) 15) (cons (find-char-equivalent (+ (find-number-equivalent (car n)) 1)) (cdr n)))
            (else (cons 0 (increment-b (cdr n)))))))
  
  (define successor-b
    (lambda (n)
      (increment-b (increment-b n))))
            
  

  ;; PROJECT 1 Part C | Write your answer below here as a comment
  ;
  ; create    : Constructor; create a new representation.
  ; is-zero-b?: Predicate, observer; checks conditions about representation and does not return any representation only bool values.
  ; successor : Constructor; create a new representation from existing representation. 
  ;
  ;
  
  ;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Don't worry about the below function, we included it to test your implemented functions and display the result in the console
  ;; As you implement your functions you can Run (the button on the top right corner) to test your implementations
  (define-syntax equal??
    (syntax-rules ()
      ((_ test-exp correct-ans)
       (let ((observed-ans test-exp))
         (if (not (equal? observed-ans correct-ans))
           (printf "Oops! ~s returned ~s, should have returned ~s~%" 'test-exp observed-ans correct-ans)
           (printf "Correct! ~s => ~s~%" 'test-exp correct-ans))))))
  
  
  ;; PROJECT 1 Part D | Remove the comments and write test cases.
  (display "First Representation Tests\n")
  (equal?? (create-a 2) '(even two))            ; should return '(even)
  (equal?? (is-zero-a? '(even two)) #f)         ; should return #f
  (equal?? (is-zero-a? '(even)) #t)             ; should return #t
  (equal?? (successor-a '(even)) '(even two))   ; should return ?
  (newline)

  
  (display "Second Representation Tests\n")
  (equal?? (create-b 480) '(0 E 1)) ; should return  '(0 E 1)
  (equal?? (is-zero-b? '(0 2)) #f) ; should return #f
  (equal?? (is-zero-b? '()) #t) ; should return #t
  (equal?? (successor-b '(E F)) '(0 0 1)) ; should return '(E F)
  (newline)
  
)