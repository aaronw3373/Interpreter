;Aaron Weinberg
;John Duffy
;4/2/2018
;EECS 345 PLCs
; If you are using racket instead of scheme, uncomment these two lines, comment the (load "simpleParser.scm") and uncomment the (require "simpleParser.scm")
 #lang racket
(require "classParser.scm")

; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
(define call/cc call-with-current-continuation)


; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file class)
    (scheme->language
     (let* ((error (lambda (env) (myerror "Continue return or break used outside of loop")))
            (throw (lambda (v env) (myerror "Uncaught exception thrown")))
            (environment (interpret-outer (parser file) (newenvironment) error error error throw (cont-init))))
       (call/cc
        (lambda (return)
          (eval-funccall '(funcall main) environment return error error throw (cont-init))
        ))))))

;outer interpreter
(define interpret-outer
  (lambda (statement-list environment return break continue throw cont)
    (cond
      ((null? statement-list) environment)
      ((eq? (caar statement-list) 'class) (interpret-outer (cdr statementlist) (interpret-class (car statement-list environment return break continue throw cont)) return break continue throw cont))
      (else (myerror "you can only declare classes in the global scope!!!!")))))

;mvalue class
(define interpret-class
  (lambda (stmt environment return break continue throw cont)
    (let* ((name (cadr stmt))
           (extends (caddr stmt))
           (parent (if (null? extends) 'null (lookup-variable (cadr extends) environment)))
           (body (cadddr stmt))
           (initial (class-new parent name))
           (class (interpret-class-statement-list body environment (cont-currclass-repl (cont-class-repl cont initial) initial)))
           )
      (insert name class environment))))

(define interpret-class-statement-list
  (lambda (stmt-list environment return break continue throw cont)
    (if (null? stmt-list)
        (cont-class cont)
        (interpret-class-statement-list (cdr stmt-list)
                                        environment
                                        (let ((class-new (Mclass (car stmt-list) environment return break continue throw cont)))
                                          (cont-currlass-repl (cont-class-repl cont class-new) class-new))))))

 ; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw cont)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw cont) return break continue throw cont))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw cont)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return cont))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment cont))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment cont))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw cont))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw cont))
      ((eq? 'continue (statement-type statement)) (continue environment cont))
      ((eq? 'break (statement-type statement)) (break environment cont))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw cont))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw cont))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw cont))
      ((eq? 'function (statement-type statement)) (interpret-funcdecl statement environment return break continue throw cont))
      ((eq? 'funcall (statement-type statement)) (interpret-funccall statement environment return break continue throw cont))
      ((eq? 'dot (statement-type statement)) (interpret-dot statement environment return break continue throw cont))
      ((eq? 'new (statement-type statement)) (interpret-new statement environment return break continue throw cont))
      (else (myerror "Unknown statement:" (statement-type statement))))))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return cont)
    (let* ((error (lambda (env) (myerror "Continue return or break used outside of loop"))))
    (return (eval-expression (get-expr statement) environment return error error error)))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment cont)
    (let* ((error (lambda (env) (myerror "Continue return or break used outside of loop"))))
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment error error error error) environment)
        (insert (get-declare-var statement) 'novalue environment)))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment cont)
    (let* ((error (lambda (env) (myerror "Continue return or break used outside of loop"))))
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment error error error error) environment))))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw cont)
    (cond
      ((eval-expression (get-condition statement) environment return break continue throw) (interpret-statement (get-then statement) environment return break continue throw cont))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw cont))
      (else environment))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw cont)
    (let* ((error (lambda (env) (myerror "Continue used outside of loop"))))
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment return break error throw cont)
                            (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw cont))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment)))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environmen
;*************what to do with cont here
(define interpret-block
  (lambda (statement environment return break continue throw cont)
    (pop-frame (interpret-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))
                                         (lambda (env) (cont (pop-frame cont)))))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw cont)
    (let* ((error (lambda (env) (myerror "Continue used outside of loop"))))
    (throw (eval-expression (get-expr statement) environment error error error throw) environment cont))))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
;*****************what to do with cont here
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list 
                                                 (get-body catch-statement) 
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw cont)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
;************************ what to do with cont here
(define interpret-try
  (lambda (statement environment return break continue throw cont)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw cont))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

;declare function
(define interpret-funcdecl
  (lambda (statement environment return break continue throw cont)
    (let ((name (get-func-name statement)))
      (insert name
              (list (get-param-list statement)
                    (get-func-body statement)
                    (lambda (environment)
                      (prepare-environment name environment))) environment))))

;function call
(define interpret-funccall
  (lambda (fcall environment return break continue throw cont)
    (begin (eval-funccall fcall environment return break continue throw cont))))

;evaluation functioncall used for main
(define eval-funccall
  (lambda (fcall environment return break continue throw cont)
    (let* ((state (lookup(get-func-name fcall) environment))
           (outerenv ((get-func-env state) environment))
           (paramvals (map (lambda (v) (eval-expression v environment return break continue throw)) (get-func-params fcall)))
           (newenv (new-params-layer (get-params state) paramvals (push-frame outerenv)))
           (error (lambda (env) (myerror "Continue return or break used outside of loop")))
           (throw (lambda (v env) (myerror "Uncaught exception thrown"))))
      (call/cc
       (lambda (return)
         (interpret-statement-list (get-body-func state) newenv return error error throw cont)
         )))))

;helper function to create a new layer with parameters and return the environment
(define new-params-layer
  (lambda (names values env)
    (cond
      ((<= (length names) 0) env)
      ((eq? (length names) (length values)) (insert-nv-pairs-env (list names values) env))
      (else (myerror "Wrong number of arguments to function")))))

;helper to insert name value pairs into env PARAMS: ((names)(values)) env
(define insert-nv-pairs-env
  (lambda (l env)
    (cond
      ((null? (car l)) env)
      (else (insert-nv-pairs-env (list (cdr (car l)) (cdr (cadr l))) (insert (caar l) (caadr l) env)     )))))



;================================ Class Shiz - fuck it ========================
(define class-new
  (lambda (daddy name)
    (list 'class daddy name
          (if (eq? daddy 'null)
              (newenvironment)
              (cont-class-fields daddy))
          (if (eq? daddy 'null)
              (newenvironment)
              (cont-class-methods daddy))
          (if (eq? daddy 'null)
              (newframe)
              (cont-class-instance-names daddy)))))

(define cont-class-parent cadr)
(define cont-class-name caddr)
(define cont-class-fields cadddr)
(define cont-class-methods (lambda (v) (list-ref v 4)))
(define cont-class-instance-names (lambda (v) (list-ref v 5)))
(define inst-class cadr)

(define interpret-dot
  (lambda (expr state cont)
    (lookup-dot-var expr state cont)))    

(define lookup-dot-var
  (lambda (expr state cont)
    (let ((inst-class (dot-inst-class (cadr expr) state (cont-currclass cont) cont)))
      (variable-lookup (caddr expr) (newenvironment) (cadr inst-class) (car inst-class)))))

(define variable-lookup cadr); TODO something important
(define dot-inst-class cadr);  TODO something important

(define class-fields-repl
  (lambda (class fields)
    (replace-in-list class 3 fields)))

(define class-methods-repl
  (lambda (class methods)
    (replace-in-list class 4 methods)))

(define class-instance-names-repl
  (lambda (class instance-names)
    (replace-in-list class 5 instance-names)))

(define Mclass
  (lambda (stmt environment return break continue throw cont)
    (cond
      ((null? stmt) (cont-class cont))
      ((list? stmt) (cond
                      ((eq? 'static-function (statement-type stmt)) (Mclass-funcdecl stmt environment #t return break continue throw cont))
                      ((eq? 'function (statement-type stmt)) (Mclass-funcdecl stmt environment #f return break continue throw cont))
                      ((eq? 'static-var (statement-type stmt)) (Mclass-staticdecl stmt environment return break continue throw cont))
                      ((eq? 'var (statement-type stmt)) (Mclass-decl stmt environment return break continue throw cont))
                      (else (error "Invalid statement when declaring class."))))
      (else (cont-class return break continue throw cont)))))
                      
(define Mclass-funcdecl
  (lambda (funcdecl environment static return break continue throw cont)
    (let* ((name (cadr funcdecl))
           (class (cont-class cont))
           (classname (caddr class)))
      (class-methods-repl
       class
       (insert (cont-class-methods class)
               name
               (list (caddr funcdecl)
                     (cadddr funcdecl)
                     (lambda (environment)
                       (let ((class (lookup environment classname)))
                         (prepare-environment classname environment)))
                     (lambda (environment)
                       (lookup environment classname))
                     (if static
                         (lambda (v) 'null)
                         (lambda (v) v))))))))

(define Mclass-decl
  (lambda (stmt environment return break continue throw cont)
    (let* ((class (cont-class cont)))
      (class-instance-names-repl
       class
       (insert (cont-class-instance-names class)
               (cadr stmt)
               (if (= 3 (length stmt))
                   (interpret-statement (caddr stmt) environment return break continue throw cont) 'undefined))))))

(define Mclass-staticdecl
  (lambda (stmt environment return break continue throw cont)
    (let* ((class (cont-class cont)))
      (class-fields-repl
       class
       (insert (cont-class-fields class)
               (cadr stmt)
               (if (= 3 (length stmt))
                   (interpret-statement (caddr stmt) environment return break continue throw cont) 'undefined))))))

;=============================================================================

;====================== Continuations ========================================
; There are a lot of them now, so we're wrapping the new ones up.

;access items in continuation list
(define cont-class car)
(define cont-inst cadr)
(define cont-currclass caddr)

(define cont-class-repl
  (lambda (cont class)
    (cons class (cdr cont))))

(define cont-inst-repl
  (lambda (cont inst)
    (replace-in-list cont 1 inst)))

(define cont-currclass-repl
  (lambda (cont currclass)
    (replace-in-list cont 2 currclass)))

(define cont-init
  (lambda ()
    (list 'null 'null 'null)))



; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment return break continue throw cont)
    (cond     
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment))
      ((eq? (operator expr) 'funcall) (eval-funccall expr environment return break continue throw cont))
      (else (eval-operator expr environment)))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment)
    (let* ((error (lambda (env) (myerror "Continue return or break used outside of loop"))))
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment error error error error)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment error error error error)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment error error error error) environment))))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment)
    (let* ((error (lambda (env) (myerror "Continue return or break used outside of loop"))))
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment error error error error)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment error error error error)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment error error error error)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment error error error error)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment error error error error)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment error error error error)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment error error error error))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment error error error error)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment error error error error)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment error error error error)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment error error error error)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment error error error error)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment error error error error)))
      (else (myerror "Unknown operator:" (operator expr)))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)
(define get-func-name operand1)
(define get-param-list operand2)
(define get-func-body operand3)
(define get-func-env operand2)
(define get-func-params cddr)
(define get-params car)
(define get-body-func cadr)
  
(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))
  
;returns the same list with a new value replacing the old value at the specified index.
(define replace-in-list
  (lambda (list i x)
    (if (= 0 i)
        (cons val (cdr list))
        (cons (car list) (replace (cdr list) (- i 1) x)))))


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

(define exists-in-frame?
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) #f)
      (else #t))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable or function is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable or function used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (cons (scheme->language val) (cdr vallist)))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Returns an appropriate environment for a given function
(define prepare-environment
  (lambda (name state)
    (if (null? state) (myerror "error: Function name not found.")
        (if(not (exists-in-frame? name (car state)))
           (prepare-environment name (cdr state)) state))))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))



; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

