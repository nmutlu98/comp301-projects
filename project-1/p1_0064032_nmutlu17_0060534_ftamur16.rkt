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
  ;
  ;
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
    (lambda (n) (append n '(two)))
    )

  ;; Second Representation | We added a -b suffix so that both Unary and BigNum can be tested at once.
  (define create-b
    ())

  (define is-zero-b?
    ())

  (define successor-b
    ())

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
  ;(equal?? (create-b ) '()) ; should return ?
  ;(equal?? (is-zero-b? '()) #f) ; should return #f
  ;(equal?? (is-zero-b? '()) #t) ; should return #t
  ;(equal?? (successor-b '()) '()) ; should return ?
  (newline)
  
)