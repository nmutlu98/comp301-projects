(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?
  (require "store.scm")                 ; for reference?

  (provide (all-defined-out))           ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean, a procval, or a
;;; reference.
  (define ref-val?
    (lambda (exp)
      (cases expval exp
        (ref-val (ref) #t)
        (else #f))))

  (define-datatype expval expval?
    (num-val
     (value number?))
    (bool-val
     (boolean boolean?))
    (proc-val 
     (proc proc?))
    (ref-val
     (ref reference?))
    (array-val
     (array array?))
    )

;;; extractors:

  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval->ref
    (lambda (v)
      (cases expval v
	(ref-val (ref) ref)
	(else (expval-extractor-error 'reference v)))))
  
  (define expval->array
    (lambda (v)
      (cases expval v
        (array-val (array) array)
        (else (expval-extractor-error 'array v)))
      )
    )

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))

;;;;;;;;;;;;;;;; arrays ;;;;;;;;;;;;;;;;;;;;


  ;; define array datatype
  (define-datatype array array?
    (an-array
     (ref reference?)
     (length (lambda (x) (and (integer? x) (positive? x))))
     )
    )

  ;; array functions

  ;; create an array with length and value
  (define (make-array length value)
    (if (positive? length)
        (let ((result (newref value)))
          (make-array-helper (- length 1) value result length))
        (eopl:error 'make-array
                    "The array length must be greater than 0.")
        )
    )

  ;; loop for creating elements of array except the zero index. 
  (define (make-array-helper length value result init-length)
    (if (= 0 length)
        (an-array result init-length)
        (begin (newref value)
               (make-array-helper (- length 1) value
                                  result init-length))
        )
    )

  ;; read element of array at specified index 
  (define (read-array array1 index)
    (cases array array1
      (an-array (ref length)
                (if (< index length)
                    (deref (+ ref index))
                    (eopl:error 'array-ref
                                "Array index out of bound."))
                )
      )
    )

  ;; update element of array at specified index
  (define (update-array array1 index value)
    (cases array array1
      (an-array (ref length)
                (if (< index length)
                    (setref! (+ ref index) value)
                    (eopl:error 'array-ref
                                "Array index out of bound."))
                )
      )
    )


  ;; get length of an array
  (define (length-array array1)
    (cases array array1
      (an-array (ref length) length))
    )

;;;;;;;;;;;;;;;; enviroment ;;;;;;;;;;;;;;;;
  
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env-rec*
      (proc-names (list-of symbol?))
      (b-vars (list-of symbol?))
      (proc-bodies (list-of expression?))
      (saved-env environment?)))

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
	(empty-env () '())
	(extend-env (sym val saved-env)
	  (cons
	    (list sym (expval->printable val))
	    (env->list saved-env)))
	(extend-env-rec* (p-names b-vars p-bodies saved-env)
	  (cons
	    (list 'letrec p-names '...)
	    (env->list saved-env))))))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned
  ;; up with env->list 
  (define expval->printable
    (lambda (val)
      (cases expval val
	(proc-val (p)
	  (cases proc p
	    (procedure (var body saved-env)
	      (list 'procedure var '... (env->list saved-env)))))
	(else val))))


)
