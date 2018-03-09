#lang racket
(require "simpleParser.scm")
;Interpreter EECS 345
;Aaron Weinberg and Jonathan Duffy

;exprLis - parsed program of expressions
;expr - one expression from the parsed program
;state - state of variables and values

;;;;;;;;;;;;;;;;;;;;;main functions ;;;;;;;;;;;;;

;Main Function that takes a filename and returns the result of the program
;call (interpret "TestCode.txt")
(define interpret
  (lambda (filename)
    (call/cc
     (lambda (return)
       (M_Forward_OP (parser filename) (state_new) return #f #f)))))
       

;this is our M_State function, takes a parse tree fragment
;returns the state
(define M_Forward_OP 
  (lambda (expr state return break continue)
    (cond
      ((null? expr) state)
      ((list? expr) (cond
                      ((list? (get_op expr)) (M_Program expr state return break continue)) ;if the expression is a list of expressions call M_Program
                      ;((eq?   (get_op expr) 'begin)    (M_block expr state return break continue))
                      ;((eq?   (get_op expr) 'break)    (M_break state))
                      ;((eq?   (get_op expr) 'continue) (M_continue state))
                      ;((eq?   (get_op expr) 'try)      (M_try expr state return break continue))
                      ((eq?   (get_op expr) 'var)      (declaration_OP expr state return break continue))
                      ((eq?   (get_op expr) '=)        (assignment_OP expr state return break continue))
                      ((eq?   (get_op expr) 'return)   (return_OP expr state return break continue))
                      ;((eq?   (get_op expr) 'if)       (if_OP expr state return_b return_v))  ;fix
                      ;((eq?   (get_op expr) 'while)    (while_OP expr state return_b return_v))  ;fix
                      ;((eq?   (get_op expr) 'throw)    (M_throw expr state return break continue))
                      (else   (error "Invalid Expression: " expr)))) ;invalid operation
      (else state))))
       
;M_Program executes on a list of statements, doesn't add a layer just executes the list of statements in order
;reutrns state
(define M_Program
  (lambda (exprLis state return break continue)
    (cond
      ((null? exprLis) state)
      (else (M_Program (cdr exprLis) (M_Forward_OP (car exprLis) state return break continue) return break continue)))))



;;;;;;;;;;;;;;;;operations ;;;;;;;;;;;;;;
;Return
;returns calls return (referencing first call/cc in interpret to return the value from the program)
(define return_OP
  (lambda (expr state return break continue)
    (return (return_value (M_arith_eval (cadr expr) state return break continue)))))

;helper since returns can be integers and booleans
(define return_value
  (lambda (expr)
    (cond
      ((eq? expr #t) 'true)
      ((eq? expr #f) 'false)
      (else expr))))
    
;Declaration (var variable (value optional))
(define declaration_OP
  (lambda (expr state return break continue)
    (cond
      ((m_member? state (get_var expr)) (error "Variable already declared"))
      ((= 3 (length expr)) ;assignment too.
       (state_bind state (get_var expr) (M_arith_eval (get_val expr) state  return break continue)))
      (else (state_bind state (get_var expr) 'undefined)))))

;Assignment (= variable expression) changes value of var to val in state
(define assignment_OP
  (lambda (expr state  return break continue)
    (if (m_member? state (get_var expr))
        (m_update state (get_var expr) (M_arith_eval(get_val expr) state return break continue))
        (error "Asssigning varible before declaration" (get_var expr)))))

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


;;;;;;;M_Value functions;;;;;;;;;;;

;M_arith_eval - Function that takes a simple or compound arithmetic expression (* + - / %) and returns the proper value or sends to M_Boolean
(define M_arith_eval
  (lambda (expr state return break continue)
    (cond
      ((number? expr) expr ) ;if number, return it.
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((list? expr) 
            (cond
              ;arithmatic functions
              ((eq? (op expr) '*)(* (M_arith_eval (arg1 expr) state return break continue)(M_arith_eval (arg2 expr) state return break continue)))
              ((eq? (op expr) '+)(+ (M_arith_eval (arg1 expr) state return break continue)(M_arith_eval (arg2 expr) state return break continue)))
              ((eq? (op expr) '-) (if (null? (cddr expr))
                                    (- 0 (M_arith_eval (arg1 expr) state return break continue))
                                    (- (M_arith_eval (arg1 expr) state return break continue)(M_arith_eval (arg2 expr) state return break continue))))
              ((eq? (op expr) '/)(quotient (M_arith_eval (arg1 expr) state return break continue)(M_arith_eval (arg2 expr) state return break continue)))
              ((eq? (op expr) '%)(remainder (M_arith_eval (arg1 expr) state return break continue)(M_arith_eval (arg2 expr) state return break continue)))                
              ;forward onto boolean functions
              ((or (eq? (op expr) '==) (or (eq? (op expr) '!=) (or (eq? (op expr) '>) (or (eq? (op expr) '<) (or (eq? (op expr) '>=) (or (eq? (op expr) '<=) (or (eq? (op expr) '&&) (or (eq? (op expr) '||) (or (eq? (op expr) '!))))))))))
                   (M_Boolean expr state return break continue))                   
              (else(error "Invalid operation in: " expr)))) ;throw error if operator isn't one of those operators.
      (else (M_Var_Value expr state))))) ;look up the value of the variable

;takes an expression and state and returns a state and value
(define M_Boolean
 (lambda (expr state return break continue)
   (cond
     ((eq? expr 'true) #t)
     ((eq? expr 'false) #f)
     ((list? expr) (cond
                   ((eq? (op expr) '==) (eq? (M_arith_eval (arg1 expr) state return break continue) (M_arith_eval (arg2 expr) state return break continue)))
                   ((eq? (op expr) '!=) (not (eq? (M_arith_eval (arg1 expr) state return break continue) (M_arith_eval (arg2 expr) state return break continue))))
                   ((eq? (op expr) '<) (< (M_arith_eval (arg1 expr) state return break continue) (M_arith_eval (arg2 expr) state return break continue)))
                   ((eq? (op expr) '>) (> (M_arith_eval (arg1 expr) state return break continue) (M_arith_eval (arg2 expr) state return break continue)))
                   ((eq? (op expr) '<=) (<= (M_arith_eval (arg1 expr) state return break continue) (M_arith_eval (arg2 expr) state return break continue)))
                   ((eq? (op expr) '>=) (>= (M_arith_eval (arg1 expr) state return break continue) (M_arith_eval (arg2 expr) state return break continue)))                 
                   ((eq? (op expr) '&&) (and (M_Boolean (arg1 expr) state return break continue) (M_Boolean (arg2 expr) state return break continue)))
                   ((eq? (op expr) '||) (or (M_Boolean (arg1 expr) state return break continue) (M_Boolean (arg2 expr) state return break continue)))
                   ((eq? (op expr) '!) (not (M_Boolean (arg1 expr) state return break continue)))
                   (else (error "Invalid Condition: " expr))))
       (else (M_Var_Value expr state)))))

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
    (if (l_member? (car state) name) error "Variable already declared")
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
      ((eq? (l_lookup (car s) var) 'undefined)(m_member? (cdr s) var))
      (else #t))))

;M_Var_Value takes a variable name and a state, and returns the value associated with that variable.
(define M_Var_Value
  (lambda (name state)
    (cond ((m_empty? state) (error "That variable does not exist."))
          ((eq? (l_lookup (car state) name) 'undefined) (M_Var_Value name (cdr state)))
          (else (l_lookup name (car state))))))

; update a binding for an existing variable
(define m_update
  (lambda (s var val)
    (cond
      ((null? s) (error "Variable binding not found."))
      ((eq? (l_lookup (car s) var) 'undefined) (cons (car s) (m_update (cdr s) var val)))
      (else (cons (l_add (l_rem (car s) var) var val) (cdr s))))))

;adds a layer to the state
(define m_add_layer
  (lambda (state)
    (cons (l_new) state)))

;remove layer from state
(define m_remove_layer cdr)

;executes a block of statements, in its own layer
(define m_block
  (lambda (block state return break continue)
    (m_remove_layer (M_Program (cdr block) (l_add state) return
                               (lambda (k) (break (l_rem k)))
                               (lambda (k) (continue (l_rem k)))))))

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
      ((l_null? l) 'undefined)
      ((equal? var (headvar l)) (headvar l))
      (else (l_lookup (l_cdr l) var)))))

 ; return true if var is a member of the layer
(define l_member?
  (lambda (l var)
    (not (= -1 (get_index (car l) var)))))

 ;return index of a given symbol in a list
(define get_index
        (lambda (e lst)
                (if (null? lst)  -1
                        (if (eq? (car lst) e) 0
                                (if (= (get_index e (cdr lst)) -1) -1
                                        (+ 1 (get_index e (cdr lst))))))))
