#lang eopl

;; data structures for let-lang.

(provide (all-defined-out))               ; too many things to list


;; define expression and program

; define a program
(define-datatype program program?
  (a-program
    (exp1 expression?))
)

; define expression 
(define-datatype expression expression?

  (const-exp
    (num number?))

  (str-exp
    (str symbol?))

  (op-exp
    (exp1 expression?)
    (exp2 expression?)
    (num number?))

  (zero?-exp
    (exp1 expression?))

  (if-exp
    (exp1 expression?)
    (exp2 expression?)
    (exp3 expression?)
    (exp4 expression?))

  (var-exp
    (var identifier?))

  (let-exp
    (var identifier?)
    (exp1 expression?)
    (body expression?))

)

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

(define-datatype expval expval?
  (num-val
    (value number?))
  (str-val
    (value symbol?))
  (bool-val
    (boolean boolean?)))

;(define-dataype bool-val bool-val?
;  (bool-val boolean?))

;;; extractors:

;; expval->num : ExpVal -> Int
;; Page: 70
(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

(define expval->str
  (lambda (v)
    (cases expval v
      (str-val (str) str)
      (else (expval-extractor-error 'str v)))))

;; expval->bool : ExpVal -> Bool
;; Page: 70
(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

;; example of a data type built without define-datatype

(define empty-env-record
  (lambda () 
    '()))

(define extended-env-record
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

(define empty-env-record? null?)

(define environment?
  (lambda (x)
    (or (empty-env-record? x)
        (and (pair? x)
             (symbol? (car (car x)))
             (expval? (cadr (car x)))
             (environment? (cdr x))))))

(define extended-env-record->sym
  (lambda (r)
    (car (car r))))

(define extended-env-record->val
  (lambda (r)
    (cadr (car r))))

(define extended-env-record->old-env
  (lambda (r)
    (cdr r)))
