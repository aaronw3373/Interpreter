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
    (call/cc
     (lambda (return)
       (M_Forward_OP (parser filename) (state_new) return #f #f)
       

;this is our M_State function, takes a parse tree fragment
;returns the state
(define M_Forward_OP 
  (lambda (expr state return break continue)
    (cond
      ((null? expr) state)
      ((list? expr) (cond
                      ((list? (get_op expr)) (M_Program stmt state return break continue)) ;if the expression is a list of expressions call M_Program
                      ((eq?   (get_op expr) 'begin)    (M_block expr state return break continue))
                      ((eq?   (get_op expr) 'break)    (M_break state))
                      ((eq?   (get_op expr) 'continue) (M_continue state))
                      ((eq?   (get_op expr) 'try)      (M_try expr state return break continue))
                      ((eq?   (get_op expr) 'var)      (cons (declaration_OP  expr state)  (cons return_b (cons return_v '())))) ; fix
                      ((eq?   (get_op expr) '=)        (cons (assignment_OP (get_var expr) (get_val expr) state) (cons return_b (cons return_v '())))) ; fix
                      ((eq?   (get_op expr) 'return)   (cons (get_op (return_OP expr state)) (cons #t (cons (get_var (return_OP expr state)) '()))))   ;fix
                      ((eq?   (get_op expr) 'if)       (if_OP expr state return_b return_v))  ;fix
                      ((eq?   (get_op expr) 'while)    (while_OP expr state return_b return_v))  ;fix
                      (else   (error "Invalid Expression: " expr)))) ;invalid operation
      (else state))))
       
;M_Program executes on a list of statements, doesn't add a layer just executes the list of statements in order
;reutrns state
(define M_Program
  (lambda (exprLis state return break continue)
    (cond
      ((null? exprLis) state)
      (else (M_Program (cdr exprLis) (M_Forward_OP (car exprLis) state return break continue) return break continue)))))


	 



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

;--------- ABSTRACTIIONS---------
;defining order
(define op car)     ;used in m_arith_eval and m_boolean
(define arg1 cadr)  ;used in m_arith_eval and m_boolean
(define arg2 caddr) ;used in m_arith_eval and m_boolean
(define get_state car)        ;used in interpret and M_Program
(define get_return_b cadr)    ;used in interpret and M_Program
(define get_return_v caddr)   ;used in interpret and M_Program
(define get_program_head car) ;used in interpret and M_Program
(define get_program_tail cdr) ;used in interpret and M_Program
(define get_op car)     ;used in M_forward_OP
(define get_var cadr)   ;used in M_forward_OP
(define get_val caddr)  ;used in M_forward_OP


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
    (cons (l_add (car state) name value) (cdr state))))

;remove first occurance of the variable from the state
(define m_remove
  (lambda (name state)
    (cond ((m_empty? state) state)
          ((eq? name (caar state)) (m_cdr state) name)
          (else (state_bind (m_remove (m_cdr state) name) (caar state) (caadr state))))))

;returns if a symbol is in a list
(define m_member?
  (lambda (s var)
    (cond
      ((null? s) #f)
      ((eq? (l_lookup (car s) var) 'not_found)(m_member? (cdr s) var))
      (else #t))))

;M_Var_Value takes a variable name and a state, and returns the value associated with that variable.
(define M_Var_Value
  (lambda (name state)
    (cond ((m_empty? state) (error "That variable does not exist."))
          ((and (eq? (caar state) name) (eq? (caadr state) 'undefined)) (error "That variable is undefined"))
          ((eq? (car (car state)) name) (caar (cdr state)))
          (else (M_Var_Value name (m_cdr state))))))

; update a binding for an existing variable
(define m_update
  (lambda (s var val)
    (cond
      ((null? s) (error "Variable binding not found."))
      ((eq? (l_lookup (car s) var) 'not_found) (cons (car s) (m_update (cdr s) var val)))
      (else (cons (l_add (l_rem (car s) var) var val) (cdr s))))))

;adds a layer to the state
(define m_add_layer
  (lambda (state)
    (cons (l_new) state)))

;remove layer from state
(define m_remove_layer cdr)

;Layer functions. -------------------------------------------
;make a new layer
(define l_new
  (lambda () '(() ())))

;get head variable of layer
(define headvar caar)

;get value of head variable of layer
(define headval caadr)

;add a variable and value pair to a layer
(define l_add
  (lambda (l var val)
    (list (cons var (car l)) (cons val (cadr l)))))

;cdr of the layer
(define l_cdr
  (lambda (l)
    (null? (car l))))

;null? for layer
(define l_null?
  (lambda (l)
    (null (car l))))

;remove first occurance of var from layer
(define l_rem
  (lambda (l var)
    (cond
      ((l_null? l) l)
      ((eq? var (headvar l)) (l_cdr l))
      (else (l_add (l_rem (l_cdr l) var) (headvar l) (headval l))))))

;lookup binding for var in the layer
(define l_lookup
  (lambda (l var)
    (cond
      ((l_null? l) 'var_not_found)
      ((equal? var (headvar l)) (headvar l))
      (else (l_lookup (l_cdr l) var)))))
