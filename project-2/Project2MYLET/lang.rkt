#lang eopl

;; grammar for the LET language  

(provide (all-defined-out))

;;;;;;;;;;;;;;;; part: grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (string ("'" letter (arbno (or letter whitespace digit "_" "-" "?")) "'") symbol)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)
    
    (expression (number) const-exp)

    (expression (string) str-exp)
    
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression (identifier) var-exp)
    
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
     ("if" expression "then" expression (arbno "elif" expression "then" expression) "else" expression)
     if-exp)
    
    (expression
     ("op" "(" expression "," expression "," number ")")
     op-exp)

    (expression
     ("logical-op" "(" expression "," expression "," number ")") log-exp)

    ))


;;;;;;;;;;;;;;;; part: sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

