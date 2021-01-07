(module interp (lib "eopl.ss" "eopl")
  
  ;; cps interpreter for the LETREC language, using the data structure representation of continuations

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of/k)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> FinalAnswer
  ;; Page: 143 and 154
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of/k exp1 (init-env) (end-cont))))))  

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 143--146, and 154
  (define value-of/k
    (lambda (exp env cont)
      (cases expression exp
        (const-exp (num) (apply-cont cont (num-val num)))
        (var-exp (var) (apply-cont cont (apply-env env var)))
        (proc-exp (var body)
          (apply-cont cont 
            (proc-val (procedure var body env))))
        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k letrec-body
            (extend-env-rec p-name b-var p-body env)
            cont))
        (zero?-exp (exp1)
          (value-of/k exp1 env
            (zero1-cont cont)))
        (let-exp (var exp1 body)
          (value-of/k exp1 env
            (let-exp-cont var body env cont)))
        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 env cont)))
        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont exp2 env cont)))        
        (call-exp (rator rand) 
          (value-of/k rator env
            (rator-cont rand env cont)))


        ;;;; TASK 4 ;;;;;;;;;
        ; Implement car expression case here
       (car-exp (exp)
                (value-of/k exp env
                            (car-cont cont)))
                
        ; Implement cdr expression case here
       (cdr-exp (exp)
                (value-of/k exp env
                            (cdr-cont cont)))
        ; Implement null? expression case here
       (null?-exp (exp)
                  (value-of/k exp env
                              (null?-cont cont)))
        ; Implement emptylist expression case here
       (emptylist-exp ()
                      (apply-cont cont (emptylist-val)))
        ; Implement your list expression case here
        (list-exp (exps)
              (if (null? exps)
                  (apply-cont cont (emptylist-val))
                  (value-of/k (car (reverse exps)) env (list-cont '() (cdr (reverse exps)) env cont))))
    
        ; Implement the map expression case here        
        (map-exp (proc-exp lst-exp)
                 (value-of/k proc-exp env (map1-cont lst-exp env cont)))
        ;;;;;;;;;;;;;;;;;;;;;;
   )))

  ;; apply-cont : Cont * ExpVal -> FinalAnswer
  ;; Page: 148
  (define apply-cont
    (lambda (cont val)
      (cases continuation cont
        (end-cont () 
          (begin
            (eopl:printf
              "End of computation.~%")
            val))
        (zero1-cont (saved-cont)
          (apply-cont saved-cont
            (bool-val
              (zero? (expval->num val)))))
        (let-exp-cont (var body saved-env saved-cont)
          (value-of/k body
            (extend-env var val saved-env) saved-cont))
        (if-test-cont (exp2 exp3 saved-env saved-cont)
          (if (expval->bool val)
             (value-of/k exp2 saved-env saved-cont)
             (value-of/k exp3 saved-env saved-cont)))
        (diff1-cont (exp2 saved-env saved-cont)
          (value-of/k exp2
            saved-env (diff2-cont val saved-cont)))
        (diff2-cont (val1 saved-cont)
          (let ((num1 (expval->num val1))
                (num2 (expval->num val)))
            (apply-cont saved-cont
              (num-val (- num1 num2)))))
        (rator-cont (rand saved-env saved-cont)
          (value-of/k rand saved-env
            (rand-cont val saved-cont)))
        (rand-cont (val1 saved-cont)
          (let ((proc (expval->proc val1)))
            (apply-procedure/k proc val saved-cont)))

        ;;;;;;;;;;;;;;;;;;;;;;; TASK 5 ;;;;;;;;;;;;;;;;;;;;;;;;
        ; implement "car-cont" continuation here
        (car-cont (saved-cont)
                  (apply-cont saved-cont (expval->car val)))
        ; implement "cdr-cont" continuation here
       (cdr-cont (saved-cont)
                 (apply-cont saved-cont (expval->cdr val)))
        ; implement "null?-cont" continuation here
       (null?-cont (saved-cont)
                   (apply-cont saved-cont (bool-val (expval->null? val))))
        
        ; implement continuation for list-exp here.
        ; hint: you will need to call value-of/k recursively, by passing this continuation as cont to value-of/k.
        (list-cont (vals remainings saved-env saved-cont)
                   (if (null? remainings)
                       (apply-cont saved-cont (pair-val val (get-list vals)))
                       (value-of/k (car remainings) saved-env (list-cont (cons val vals) (cdr remainings) saved-env saved-cont))))
        ; implement map-exp continuation(s) here. you will notice that one continuation will not be enough.
        (map1-cont (lst saved-env saved-cont)
                   (value-of/k lst saved-env (map2-cont val saved-env saved-cont)))
        (map2-cont (proc saved-env saved-cont)
                   (if (expval->null? val)
                       val
                       (apply-procedure/k (expval->proc proc) (expval->car val) (map3-cont '() (expval->cdr val) proc saved-env saved-cont))))
        (map3-cont (res remainings proc saved-env saved-cont)
                   (if (expval->null? remainings)
                       (let ((result (reverse(cons val res))))
                         (apply-cont saved-cont (get-list result)))
                        (apply-procedure/k (expval->proc proc) (expval->car remainings) (map3-cont (cons val res) (expval->cdr remainings) proc saved-env saved-cont))))
                     
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        )))

  
  (define get-list
    (lambda (values)
      (if (null? values)
          (emptylist-val)
          (pair-val (car values) (get-list (cdr values))))))
  
  

  ;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
  ;; Page 152 and 155
  (define apply-procedure/k
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var arg saved-env)
            cont)))))
  
  )
  


  
