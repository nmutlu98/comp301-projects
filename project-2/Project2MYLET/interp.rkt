#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of-if : if-exp -> ExpVal
(define value-of-if
  (lambda (conds exps exp env)
    (cond ((null? conds) (value-of exp env))
          ((expval->bool (value-of (car conds) env)) (value-of (car exps) env))
          (else (value-of-if (cdr conds) (cdr exps) exp env)))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      ;;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
      (const-exp (num) (num-val num))
      
      ; string expressions.
      (str-exp (str) (str-val (apply string-append (map symbol->string (list str)))))

      ;;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
      (var-exp (var) (apply-env env var))
     
      ;;\commentbox{\zerotestspec}
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))

      ;;\commentbox{\ma{\theletspecsplit}}
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))
                           
      (if-exp (exp1 exp2 conds exps exp3)
              (let ((cond1 (value-of exp1 env)))
                (let ((cond1-bool (expval->bool cond1)))
                  (if cond1-bool
                      (value-of exp2 env)
                      (value-of-if conds exps exp3 env)))))
      
      (op-exp (exp1 exp2 operation)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                (let ((num1 (expval->num val1))
                      (num2 (expval->num val2)))
                  (cond ((eq? operation 1) (num-val (+ num1 num2)))
                        ((eq? operation 2) (num-val (* num1 num2)))
                        ((eq? operation 3) (num-val (/ num1 num2)))
                        (else (num-val (- num1 num2)))))))
      
      (log-exp (exp1 exp2 operation)
               (let ((val1 (value-of exp1 env))
                     (val2 (value-of exp2 env)))
                 (let ((bool1 (expval->bool val1))
                       (bool2 (expval->bool val2)))
                   (if (eq? operation 1)
                       (bool-val (and bool1 bool2))
                       (bool-val (or bool1 bool2))))))
                        
      )))