(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))

        (newarray-exp (num1 num2)
                  (arr-val (helper-new-array num1 (num-val num2))))

        (update-array-exp (array num1 exp1)
                          (let ((val (value-of exp1 env))
                                (sc-array (expval->arr (value-of array env))))
                            (helper-update-array sc-array num1 val)
                            (num-val 23)))

        (read-array-exp (array num1)
                        (let ((arr (expval->arr (value-of array env))))
                          (helper-read-array arr num1)))

        (newqueue-exp ()
                      (value-of (newarray-exp 1001 -1) env))

        (queue-push-exp (q exp)
                        (let ((queue (value-of q env)))
                          (let ((val (value-of exp env))
                                (empty-index (first-empty-index-in-array (expval->arr queue))))
                                (helper-update-array (expval->arr queue) empty-index val))))
        
        
        (empty-queue?-exp (q)
                          (if (helper-empty-array (expval->arr (value-of q env)))
                              (bool-val #t)
                              (bool-val #f)
                          ))
                        
        (queue-pop-exp (q)
                       (let ((queue (expval->arr (value-of q env))))
                         (if (helper-empty-array queue)
                             (num-val -1)
                             (let ((firstval (deref (expval->ref (car queue)))))
                               (helper-pop-queue queue)
                               firstval))))
                               
        
        (queue-top-exp (q)
                      (let ((queue (expval->arr (value-of q env))))
                         (if (helper-empty-array queue)
                             (num-val -1)
                             (let ((firstval (deref (expval->ref (car queue)))))
                               firstval))))
        
        (queue-size-exp (q)
                     (let ((queue (expval->arr (value-of q env))))
                       (num-val (array-size queue))))
        
        (print-queue-exp (q)
                     (let ((queue (expval->arr (value-of q env))))
                       (helper-print-array queue)))

        (newstack-exp ()
                      (value-of (newarray-exp 1001 -1) env))

        (stack-push-exp (s exp1)
                    (let ((stack (expval->arr (value-of s env))))
                          (let ((val (value-of exp1 env))
                                (empty-index (first-empty-index-in-array stack)))
                                (helper-update-array stack empty-index val))))

        (stack-pop-exp (s)
                       (let ((stack (expval->arr (value-of s env))))
                         (let ((size (array-size stack)))
                           (let ((pop-val (helper-read-array stack (- size 1))))
                             (helper-update-array stack (- size 1) (num-val -1))
                             pop-val))))

        (stack-top-exp (s)
                       (let ((stack (expval->arr (value-of s env))))
                         (let ((size (array-size stack)))
                           (let ((top-val (helper-read-array stack (- size 1))))
                             top-val))))

        (stack-size-exp (s)
                        (let ((stack (expval->arr (value-of s env))))
                          (num-val (array-size stack))))

        (empty-stack?-exp (s)
                          (let ((stack (expval->arr (value-of s env))))
                            (if (eq? (array-size stack) 0)
                                (bool-val #t)
                                (bool-val #f))))

        (print-stack-exp (s)
                         (let ((stack (expval->arr (value-of s env))))
                           (helper-print-array stack)))
        
        
        )))

  ; helper-new-array: integer * expval -> (list-of ref-val)
  ; usage: used to create a new array with num1 elements where each element's value is num2
  
  (define helper-new-array
    (lambda (num1 num2)
      (if (eq? num1 0)
          '()
          (cons (ref-val (newref num2)) (helper-new-array (- num1 1) num2)))))

  ; helper-update-array: (list-of ref-val) * integer * expval -> returns nothing
  ; usage: used to update the value in the indexth element of the given array. The new value stored in the indexth element becomes value.
  
  (define helper-update-array
    (lambda (array index value)
      (if (eq? index 0)
          (setref! (expval->ref (car array)) value)
          (helper-update-array (cdr array) (- index 1) value))))

  ; helper-read-array: (list-of ref-val) * integer -> expval
  ; usage: used to get the value stored in the given index of the array
  
  (define helper-read-array
    (lambda (arr index)
      (if (eq? index 0)
          (deref (expval->ref (car arr)))
          (helper-read-array (cdr arr) (- index 1)))))

  ; array-size: (list-of ref-val) -> integer
  ; usage: used to get the number of elements in the queue
  (define array-size
    (lambda (arr)
    (if (helper-empty-array arr)
          0
          (+ 1 (array-size (cdr arr))))))
  
  ; first-empty-index-in-array: (list-of ref-val) -> integer
  ; usage: used to get the index of the first empty cell in the queue
  
  (define first-empty-index-in-array
    (lambda (arr)
      (if (eq? (expval->num (deref (expval->ref (car arr)))) -1)
          0
          (+ 1 (first-empty-index-in-array (cdr arr))))))

  ; helper-pop-queue: (list-of ref-val) -> returns nothing
  ; usage: used to slide all the elements to the left after an element is popped from the queue
  
  (define helper-pop-queue
    (lambda (arr)
      (define helper
        (lambda (current-index size)
          (if (eq? current-index (- size 1))
              (helper-update-array arr current-index (num-val -1))
              (begin
                (helper-update-array arr current-index (deref (expval->ref (list-ref arr (+ current-index 1)))))
                (helper (+ current-index 1) size)))))
      (let ((size (array-size arr)))
        (helper 0 size))))
      
  ; helper-print-array: (list-of ref-val) -> returns nothing
  ; usage: used to traverse all the elements in the queue and make a string from those elements seperated by spaces. Then prints the string with display.
  
  (define helper-print-array
    (lambda (arr)
      (define helper
        (lambda (lst remaining-arr)
          (if (helper-empty-array remaining-arr)
              (map number->string lst)
              (helper (append lst (list (expval->num (deref (expval->ref (car remaining-arr)))))) (cdr remaining-arr)))))
      (display (helper '() arr))))

  ; helper-empty-array: (list-of ref-val) -> returns boolean
  ; usage: used to check whether an array has empty cells or not. returns true if it has, returns false otherwise.
  
  (define helper-empty-array
    (lambda (arr)
          (cond ((null? arr) #t)
                ((eq? (expval->num (deref (expval->ref (car arr)))) -1) (helper-empty-array (cdr arr)))
                ((> (expval->num (deref (expval->ref (car arr)))) 0) #f))))
     
  

    


  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))

  
  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 
  )
  


  
