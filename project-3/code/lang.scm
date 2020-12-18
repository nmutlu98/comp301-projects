(module lang (lib "eopl.ss" "eopl")                

  ;; language for EXPLICIT-REFS
  
  (require "drscheme-init.scm")
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (expression) a-program)

      (expression (number) const-exp)
      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression (identifier) var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)   

      (expression
       ("proc" "(" identifier ")" expression)
       proc-exp)

      (expression
       ("(" expression expression ")")
       call-exp)

      (expression
        ("letrec"
          (arbno identifier "(" identifier ")" "=" expression)
           "in" expression)
        letrec-exp)
      
      ;; new for explicit-refs

      (expression
        ("begin" expression (arbno ";" expression) "end")
        begin-exp)

      (expression
        ("newref" "(" expression ")")
        newref-exp)

      (expression
        ("deref" "(" expression ")")
        deref-exp)

      (expression
        ("setref" "(" expression "," expression ")")
        setref-exp)

      ;; new for array operations

      (expression
       ("newarray" "(" number "," number ")")
       newarray-exp)

      (expression
       ("update-array" "(" expression "," number "," expression ")")
       update-array-exp)

      (expression
       ("read-array" "(" expression "," number ")")
       read-array-exp)

      (expression ("newqueue" "(" ")") newqueue-exp)

      (expression ("queue-push" "(" expression "," expression ")") queue-push-exp)

      (expression ("queue-pop" "(" expression ")") queue-pop-exp)

      (expression ("queue-size" "(" expression ")") queue-size-exp)

      (expression ("queue-top" "(" expression ")") queue-top-exp)
      
      (expression ("empty-queue?" "(" expression ")") empty-queue?-exp)

      (expression ("print-queue" "(" expression ")") print-queue-exp)

      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
