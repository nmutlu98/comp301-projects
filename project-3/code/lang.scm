(module lang (lib "eopl.ss" "eopl")                

  ;; language for EXPLICIT-REFS
  
  (require "drscheme-init.scm")
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
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

      ;; array expressions

      ;; syntax -> newarray(size, default-value)
      (expression
       ("newarray" "(" expression "," expression ")") newarray-exp)

      ;; syntax -> update-array(array, index, value)
      (expression
       ("update-array" "(" expression "," expression "," expression ")") updatearray-exp)

      ;; syntax -> read-array(array, index)
      (expression
       ("read-array" "(" expression "," expression ")") readarray-exp)

      ;; syntax -> length-array(array)
      (expression
       ("length-array" "(" expression ")") lengtharray-exp)

      ;; stack expressions

      ;; syntax -> newstack()
      (expression
        ("newstack" "(" ")") newstack-exp)

      ;; syntax -> stack-push(stack, value)
      (expression
        ("stack-push" "(" expression "," expression ")") stackpush-exp)

      ;; syntax -> stack-pop(stack)
      (expression
        ("stack-pop" "(" expression ")") stackpop-exp)

      ;; syntax -> stack-size(stack)
      (expression
        ("stack-size" "(" expression ")") stacksize-exp)

      ;; syntax -> stack-top(stack)
      (expression
        ("stack-top" "(" expression ")") stacktop-exp)

      ;; syntax -> empty-stack?(stack)
      (expression
        ("empty-stack?" "(" expression ")") stackempty-exp)

      ;; syntax -> print-stack(stack)
      (expression
        ("print-stack" "(" expression ")") stackprint-exp)

      ;; queue expressions

      ;; syntax -> newqueue()
      (expression
        ("newqueue" "(" ")") newqueue-exp)

      ;; syntax -> queue-push(queue, value)
      (expression
        ("queue-push" "(" expression "," expression ")") queuepush-exp)

      ;; syntax -> queue-pop(queue)
      (expression
        ("queue-pop" "(" expression ")") queuepop-exp)

      ;; syntax -> queue-size(queue)
      (expression
        ("queue-size" "(" expression ")") queuesize-exp)

      ;; syntax -> queue-top(queue)
      (expression
        ("queue-top" "(" expression ")") queuetop-exp)

      ;; syntax -> empty-queue?(queue)
      (expression
        ("empty-queue?" "(" expression ")") queueempty-exp)

      ;; syntax -> print-queue(queue)
      (expression
        ("print-queue" "(" expression ")") queueprint-exp)

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
