(module tests mzscheme
  
  (provide test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
  
      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "-(44,33)" 11)
  
      ;; nested arithmetic
      (nested-arith-left "-(-(44,33),22)" -11)
      (nested-arith-right "-(55, -(22,11))" 44)
  
      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "-(x,1)" 9)
      (test-var-3 "-(1,x)" -9)
      
      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "-(x,foo)" error)
  
      ;; simple conditionals
      (if-true "if zero?(0) then 3 else 4" 3)
      (if-false "if zero?(1) then 3 else 4" 4)


      ;; ======================= PARAMETER PASSING - TASK 3 ========================

      ;; Write the expression that evaluates different for:

      ;; --- Call-by-reference and Call-by-name here
      (reference-versus-name "let x=5 in let y=4 in let f=proc(y) begin set x=0; 27 end in let p=proc(k) if zero?(x) then x else k in (p (f y))" 27)

      ;; --- Call-by-value and Call-by-name here

      (value-versus-name "let x=5 in let f=proc(y) set y=4 in begin (f x); x end" 4)

      ;; ======================= PARAMETER PASSING - TASK 3 ========================
    )
  )
)