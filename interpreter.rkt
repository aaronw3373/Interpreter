#lang racket
(require "simpleParser.scm")
;Interpreter EECS 345
;Aaron Weinberg and Jonathan Duffy

;program - parsed program of expressions
;expr - one expression from the parsed program
;state - state of variables and values
;return_b - boolean if we are ready to return
;return_v - value of return (integer or boolean)


;Main Function that takes a filename and returns the result of the program
;call (interpret "TestCode.txt")
(define interpret
  (lambda (filename)
    (if (eq? (cadr (M_Program (parser filename)(state_new) #f 0)) #t) ;if return_b is true
        (cond
          ((eq? (caddr (M_Program (parser filename)(state_new) #f 0)) #t) 'true)  ; if return_v is #t the return 'TRUE
          ((eq? (caddr (M_Program (parser filename)(state_new) #f 0)) #f) 'false) ; if return_v is #f the return 'FALSE
          (else (caddr (M_Program (parser filename)(state_new) #f 0))))         ; else return return_v which will be an integer
        (error "No Return Value")))) ;if return_b is false then throw error        


;M_Program calls M_Forward_OP on the next expr until there returb_b is true
;reutrns (state return_b return_v)
(define M_Program
  (lambda (program state return_b return_v)
    (cond
      ((or (eq? return_b #t) (null? program)) (cons state (cons return_b (cons return_v '())))) ; nothing to evaluate or change so return as is
      (else  (M_Program (cdr program)   (car (M_Forward_OP (car program) state return_b return_v)) ;calls M_Program on the cdr of the program to step to the next expression until ther eis a return_b is true
                                       (cadr (M_Forward_OP (car program) state return_b return_v))
                                      (caddr (M_Forward_OP (car program) state return_b return_v)))))))

	  
;returns if a symbol is in a list
(define member?
  (lambda (s lis)
    (cond
      ((null? lis) #f)
      ((eq? s (car lis)) #t)
      (else (member? s (cdr lis))))))


;Takes a single operation in form of list (operation args1 args2 etc...) ard forwards the list to the correct operation
;returns state return_b return_v
(define M_Forward_OP 
  (lambda (expr state return_b return_v)
    (cond
      ((eq? return_b #t) (cons state (cons return_b (cons return_v '())))) ;if return_b is true then return state and values
      ((eq? (car expr) 'var)      (cons (declaration_OP expr state)  (cons return_b (cons return_v '()))))
      ((eq? (car expr) '=)        (cons (assignment_OP (cadr expr) (caddr expr) state) (cons return_b (cons return_v '()))))
      ((eq? (car expr) 'return)   (cons (car (return_OP expr state)) (cons #t (cons (cadr (return_OP expr state)) '()))))
      ((eq? (car expr) 'if)       (if_OP expr state return_b return_v))
      ((eq? (car expr) 'while)    (while_OP expr state return_b return_v))
      (else (error "Invalid Expression: " expr)))))


;Declaration (var variable (value optional))
(define declaration_OP
  (lambda (expr state)
    (cond
      ((member? (cadr expr) (car state)) (error "Variable already declared"))
      ((= 3 (length expr)) ;assignment too.
       (state_bind state (cadr expr) (cadr (M_arith_eval (caddr expr) state))))
      (else (state_bind state (cadr expr) 'undefined)))))

;Assignment (= variable expression) changes value of var to val in state
(define assignment_OP
  (lambda (var val state)
    (cond
      ((null? (car state)) (error "Variable not declared:" var))
      ((eq? var (caar state)) (cons (car state) (cons (cons (cadr(M_arith_eval val state)) (cdadr state)) '())))
      (else (cons (car state) (cons (cons (caadr state) (cadr (assignment_OP var (cadr(M_arith_eval val state)) (cons (cdar state) (cons (cdadr state) '()))))) '()))))))

;Return (return expression)
;returns state value
(define return_OP
  (lambda (expr state)
    (cons (car (M_arith_eval (cadr expr) state)) (cons (cadr (M_arith_eval (cadr expr) state)) '()))))

;if statement (if conditional then-statement optional-else-statement)
; returns state return_b return_v
(define if_OP
  (lambda (stmt state bool ret)
    (cond
      ((eq? bool #t) (cons state (cons bool (cons ret '()))))
      ((eq? (cadr (M_Boolean (cadr stmt) state)) #t)
       (cons     (car (M_Forward_OP (caddr stmt) state bool ret))
        (cons   (cadr (M_Forward_OP (caddr stmt) state bool ret))
         (cons (caddr (M_Forward_OP (caddr stmt) state bool ret)) '()))))
      (else (if (pair? (cdddr stmt)) ;if it has an else stmt
                 (cons (car (M_Forward_OP (cadddr stmt) state bool ret))
                (cons (cadr (M_Forward_OP (cadddr stmt) state bool ret))
               (cons (caddr (M_Forward_OP (cadddr stmt) state bool ret)) '())))
             (cons state (cons bool (cons ret '())))))))) ;no else statement

;while statement (while conditional body-statement) expr is the while loop statement.
; returns state return_b return_v
(define while_OP
  (lambda (expr state return_b return_v)
    (cond
      ((eq? return_b #t) (cons state (cons return_b (cons return_v '()))))
      ((eq? (cadr (M_Boolean (cadr expr) state)) #t)  ;if condition is true
       (while_OP   expr       (car (M_Forward_OP (caddr expr) state return_b return_v))
                             (cadr (M_Forward_OP (caddr expr) state return_b return_v))
                            (caddr (M_Forward_OP (caddr expr) state return_b return_v))))
        (else (cons (car (M_Boolean (cadr expr) state)) (cons return_b (cons return_v '())))))));else condition is false so return state


;defining order
(define op car)
(define arg1 cadr)
(define arg2 caddr)

;arith_eval - Function that takes a simple or compound arithmetic expression (* + - / %) and returns the proper return value and the state or sends to M_Boolean
;takes an expression and state and returns a state and value
(define M_arith_eval
  (lambda (expr state)
    (cond
      ((number? expr) (cons state (cons expr '())));if single number, return it.
      ((eq? expr 'true) (cons state (cons #t '())))
      ((eq? expr 'false) (cons state (cons #f '())))
      ((list? expr) 
            (cond ((eq? (op expr) '*) ;snarf underlying Scheme operators.
                   (cons state (cons (* (cadr(M_arith_eval (arg1 expr) state)) (cadr(M_arith_eval (arg2 expr) state))) '())))
                  ((eq? (op expr) '+)
                   (cons state (cons (+ (cadr(M_arith_eval (arg1 expr) state)) (cadr(M_arith_eval (arg2 expr) state))) '())))
                  ((eq? (op expr) '-) (if (null? (cddr expr))
                                          (cons state (cons (- 0 (cadr(M_arith_eval (arg1 expr) state))) '()))
                                          (cons state (cons (- (cadr(M_arith_eval (arg1 expr) state)) (cadr(M_arith_eval (arg2 expr) state))) '()))))
                  ((eq? (op expr) '/)
                   (cons state (cons (quotient (cadr(M_arith_eval (arg1 expr) state)) (cadr(M_arith_eval (arg2 expr) state))) '())))
                  ((eq? (op expr) '%)
                   (cons state (cons (remainder (cadr(M_arith_eval (arg1 expr) state)) (cadr(M_arith_eval (arg2 expr) state))) '())))
                  ((or (eq? (op expr) '==) (or (eq? (op expr) '!=) (or (eq? (op expr) '>) (or (eq? (op expr) '<) (or (eq? (op expr) '>=) (or (eq? (op expr) '<=) (or (eq? (op expr) '&&) (or (eq? (op expr) '||) (or (eq? (op expr) '!))))))))))
                   (cons (car (M_Boolean expr state)) (cons (cadr (M_Boolean expr state)) '())))                    
                  (else
                   (error "Invalid operation in: " expr))));throw error if operator isn't one of those.
      (else (cons state (cons (M_Var_Value expr state) '())))))) ;look up the value of the variable

;takes an expression and state and returns a state and value
(define M_Boolean
 (lambda (expr state)
   (cond
     ((eq? expr 'true) (cons state (cons #t '())))
     ((eq? expr 'false) (cons state (cons #f '())))
     ((list? expr) (cond
                   ((eq? (op expr) '==) (cons state (cons (eq? (cadr (M_arith_eval (arg1 expr) state)) (cadr (M_arith_eval (arg2 expr) state))) '())))
                   ((eq? (op expr) '!=) (cons state (cons (not (eq?  (cadr (M_arith_eval (arg1 expr) state)) (cadr(M_arith_eval (arg2 expr) state)))) '())))
                   ((eq? (op expr) '<) (cons state (cons (< (cadr (M_arith_eval (arg1 expr) state)) (cadr(M_arith_eval (arg2 expr) state))) '())))
                   ((eq? (op expr) '>) (cons state (cons (> (cadr (M_arith_eval (arg1 expr) state)) (cadr(M_arith_eval (arg2 expr) state))) '())))
                   ((eq? (op expr) '<=) (cons state (cons (<= (cadr (M_arith_eval (arg1 expr) state)) (cadr(M_arith_eval (arg2 expr) state))) '())))
                   ((eq? (op expr) '>=) (cons state (cons (>= (cadr (M_arith_eval (arg1 expr) state)) (cadr(M_arith_eval (arg2 expr) state))) '())))
                   ((eq? (op expr) '&&) (cons state (cons (and (cadr (M_Boolean (arg1 expr) state)) (cadr(M_Boolean (arg2 expr) state))) '())))
                   ((eq? (op expr) '||) (cons state (cons (or (cadr (M_Boolean (arg1 expr) state)) (cadr(M_Boolean (arg2 expr) state))) '())))
                   ((eq? (op expr) '!) (cons state (cons (not (cadr (M_Boolean (arg1 expr) state))) '())))
                   (else (error "Invalid Condition: " expr))))
      (else (cons state (cons (M_Var_Value expr state) '()))))))


;Mstate stuff -----------------------------------------------------

;return a new state
(define state_new
  (lambda ()
    '(() ())))

;acts as cdr for M_state
(define m_cdr
  (lambda (state)
    (list (cdar state) (cdr (cadr state)))))

;checks for an empty state
(define m_empty?
  (lambda (state)
    (null? (car state))))

;Function that binds a name and value pair to a state
(define state_bind
  (lambda (state name value)
    (list (cons name (car state)) (cons value (cadr state)))))

;remove first occurance of the variable from the state
(define m_remove
  (lambda (name state)
    (cond ((m_empty? state) state)
          ((eq? name (caar state)) (m_cdr state) name)
          (else (state_bind (m_remove (m_cdr state) name) (caar state) (caadr state))))))

;M_Var_Value takes a variable name and a state, and returns the value associated with that variable.
(define M_Var_Value
  (lambda (name state)
    (cond ((m_empty? state) (error "That variable does not exist."))
          ((and (eq? (caar state) name) (eq? (caadr state) 'undefined)) (error "That variable is undefined"))
          ((eq? (car (car state)) name) (caar (cdr state)))
          (else (M_Var_Value name (m_cdr state))))))
