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
       (M_Forward_OP (parser filename) (state_new) return #f #f #f)))))
       

;this is our M_State function, takes a parse tree fragment
;returns the state
(define M_Forward_OP 
  (lambda (expr state return break continue throw)
    (cond
      ((null? expr) state)
      ((list? expr) (cond
                      ((list? (get_op expr)) (M_Program expr state return break continue throw)) ;if the expression is a list of expressions call M_Program
                      ((eq?   (get_op expr) 'begin)    (M_block expr state return break continue throw))
                      ((eq?   (get_op expr) 'break)
                        ((if (eq? break #f) (error "Illegal use of break.")
                            (break state))))
                      ((eq?   (get_op expr) 'continue) (continue state))
                      ;((eq?   (get_op expr) 'try)      (M_try expr state return break continue throw))
                      ((eq?   (get_op expr) 'var)      (declaration_OP expr state return break continue throw))
                      ((eq?   (get_op expr) '=)        (assignment_OP expr state return break continue throw))
                      ((eq?   (get_op expr) 'return)   (return_OP expr state return break continue throw))
                      ((eq?   (get_op expr) 'if)       (if_OP expr state return break continue throw))
                      ((eq?   (get_op expr) 'while)    (while_OP expr state return break continue throw))
                      ((eq?   (get_op expr) 'throw)   (throw (m_add_layer (state_bind 'throw (M_Boolean (cadar expr) state) state_new (lambda (v) v)) state)))
                      (else   (M_arith_eval_state expr state return break continue throw)))) ; for side effects
                      ;(else   (error "Invalid Expression: " expr)))) ;invalid operation ;for no side effects
      (else state))))
       
;M_Program executes on a list of statements, doesn't add a layer just executes the list of statements in order
;reutrns state
(define M_Program
  (lambda (exprLis state return break continue throw)
    (cond
      ((null? exprLis) state)
      (else (M_Program (cdr exprLis) (M_Forward_OP (car exprLis) state return break continue throw) return break continue throw)))))


; for side effects
(define M_arith_eval_state
  (lambda (expr state return break continue throw)
    (eval_left (lambda (state expr2) (M_Forward_OP expr2 state return break continue throw)) state (get_program_tail expr))))

; for side effects
(define eval_left
  (lambda (f initial lis)
    (if (null? lis)
        initial
        (eval_left f (f initial (car lis)) (cdr lis)))))

;;;;;;;;;;;;;;;;operations ;;;;;;;;;;;;;;
;Return
;returns calls return (referencing first call/cc in interpret to return the value from the program)
(define return_OP
  (lambda (expr state return break continue throw)
    (return (return_value (M_arith_eval (cadr expr) state return break continue throw)))))

;helper since returns can be integers and booleans
(define return_value
  (lambda (expr)
    (cond
      ((eq? expr #t) 'true)
      ((eq? expr #f) 'false)
      (else expr))))
    
;Declaration (var variable (value optional))
(define declaration_OP
  (lambda (expr state return break continue throw)
    (cond
      ((m_member? state (get_var expr)) (error "Variable already declared"))
      ((= 3 (length expr)) ;assignment too.
       (state_bind state (get_var expr) (M_arith_eval (get_val expr) state return break continue throw)))
      (else (state_bind state (get_var expr) 'undefined)))))

;Assignment (= variable expression) changes value of var to val in state
(define assignment_OP
  (lambda (expr state  return break continue throw)
    (if (m_member? state (get_var expr))
        (m_update (M_Forward_OP (get_val expr) state return break continue throw) (get_var expr) (M_arith_eval (get_val expr) state return break continue throw))
        (error "Assigning varible before declaration" (get_var expr)))))

;if statement (if conditional then-statement optional-else-statement)
; returns state  ;currently no side effects
(define if_OP
  (lambda (expr state return break continue throw)
    (if (= 3 (length expr))
        ; IF
        (if (M_Boolean (condS expr) state return break continue throw)
            (M_Forward_OP (thenS expr)
                          (M_Forward_OP (condS expr) state return break continue throw);side effects
                          return break continue throw)
            (M_Forward_OP (condS expr) state return break continue throw);side effects
            )
        ; If Else
        (if (M_Boolean (condS expr) state return break continue throw)
            (M_Forward_OP (thenS expr)
                          (M_Forward_OP (condS expr) state return break continue throw);side effects
                          return break continue throw)
            (M_Forward_OP (elseS expr)
                          (M_Forward_OP (condS expr) state return break continue throw);side effects
                          return break continue throw)
            ))))


;while statement (while conditional body-statement) expr is the while loop statement.
; returns state ;currently no side effects
(define while_OP
  (lambda (expr state return break continue throw)
    (call/cc ;new continuation to fall back to
     (lambda (break_while)
       (while_loop (condW expr) (bodyW expr) state return break break_while continue throw)))))

(define while_loop
  (lambda (condition body state return break break_while continue throw)
    (if (M_Boolean condition state return break continue throw)
        (while_loop condition body
                    (call/cc (lambda (continue_while) (M_Forward_OP body
                                                         (M_Forward_OP condition state return break continue throw);side effects
                                                         return break_while continue_while throw)))
                      return break break_while continue throw)
        state))) 


;executes a block of statements, in its own layer
(define M_block
  (lambda (block state return break continue throw)
    (m_remove_layer (M_Program (cdr block) (m_add_layer state) return
                               (lambda (k) (break (m_remove_layer k)))
                               (lambda (k) (continue (m_remove_layer k))) throw))))

;Mstate for a try/catch?/finally? block
(define M_try
  (lambda (expr state return break continue throw)
    (cond
      ((null? expr) state)
      (else (M_Forward_OP (cadr expr)
                          (call/cc
                           (lambda (throw2)
                             (M_Forward_OP (car expr) state return break continue throw2)))
                          return break continue throw)
            return break continue throw))))

;;;;;;;M_Value functions;;;;;;;;;;;

;M_arith_eval - Function that takes a simple or compound arithmetic expression (* + - / %) and returns the proper value or sends to M_Boolean
(define M_arith_eval
  (lambda (expr state return break continue throw)
    (cond
      ((number? expr) expr ) ;if number, return it.
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((list? expr) 
            (cond
              ;arithmatic functions
              ((eq? (op expr) '=)(M_value_assign expr state return break continue throw))  ; for side effects. enabled but not working 100%
              ((eq? (op expr) '*)(* (M_arith_eval (arg1 expr) state return break continue throw)(M_arith_eval (arg2 expr) state return break continue throw)))
              ((eq? (op expr) '+)(+ (M_arith_eval (arg1 expr) state return break continue throw)(M_arith_eval (arg2 expr) state return break continue throw)))
              ((eq? (op expr) '-) (if (null? (cddr expr))
                                    (- 0 (M_arith_eval (arg1 expr) state return break continue throw))
                                    (- (M_arith_eval (arg1 expr) state return break continue throw)(M_arith_eval (arg2 expr) state return break continue throw))))
              ((eq? (op expr) '/)(quotient (M_arith_eval (arg1 expr) state return break continue throw)(M_arith_eval (arg2 expr) state return break continue throw)))
              ((eq? (op expr) '%)(remainder (M_arith_eval (arg1 expr) state return break continue throw)(M_arith_eval (arg2 expr) state return break continue throw)))                
              ;forward onto boolean functions
              ((or (eq? (op expr) '==) (or (eq? (op expr) '!=) (or (eq? (op expr) '>) (or (eq? (op expr) '<) (or (eq? (op expr) '>=) (or (eq? (op expr) '<=) (or (eq? (op expr) '&&) (or (eq? (op expr) '||) (or (eq? (op expr) '!))))))))))
                   (M_Boolean expr state return break continue throw))                   
              (else(error "Invalid operation in: " expr)))) ;throw error if operator isn't one of those operators.
      (else (M_Var_Value expr state))))) ;look up the value of the variable

;takes an expression and state and returns a state and value
(define M_Boolean
 (lambda (expr state return break continue throw)
   (cond
     ((eq? expr 'true) #t)
     ((eq? expr 'false) #f)
     ((list? expr) (cond
                   ((eq? (op expr) '==) (eq? (M_arith_eval (arg1 expr) state return break continue throw) (M_arith_eval (arg2 expr) state return break continue throw)))
                   ((eq? (op expr) '!=) (not (eq? (M_arith_eval (arg1 expr) state return break continue throw) (M_arith_eval (arg2 expr) state return break continue throw))))
                   ((eq? (op expr) '<) (< (M_arith_eval (arg1 expr) state return break continue throw) (M_arith_eval (arg2 expr) state return break continue throw)))
                   ((eq? (op expr) '>) (> (M_arith_eval (arg1 expr) state return break continue throw) (M_arith_eval (arg2 expr) state return break continue throw)))
                   ((eq? (op expr) '<=) (<= (M_arith_eval (arg1 expr) state return break continue throw) (M_arith_eval (arg2 expr) state return break continue throw)))
                   ((eq? (op expr) '>=) (>= (M_arith_eval (arg1 expr) state return break continue throw) (M_arith_eval (arg2 expr) state return break continue throw)))                 
                   ((eq? (op expr) '&&) (and (M_Boolean (arg1 expr) state return break continue throw) (M_Boolean (arg2 expr) state return break continue throw)))
                   ((eq? (op expr) '||) (or (M_Boolean (arg1 expr) state return break continue throw) (M_Boolean (arg2 expr) state return break continue throw)))
                   ((eq? (op expr) '!) (not (M_Boolean (arg1 expr) state return break continue throw)))
                   (else (error "Invalid Condition: " expr))))
       (else (M_Var_Value expr state)))))

;for side effects
(define M_value_assign
  (lambda (expr state return break continue throw)
      (M_arith_eval (arg2 expr) state return break continue throw)))

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
(define condS cadr)     ;used in If_OP
(define thenS caddr)    ;used in If_OP
(define elseS cadddr)   ;used in If_OP
(define condW cadr)     ;used in While_OP
(define bodyW caddr)    ;used in While_OP
;Mstate stuff -----------------------------------------------------

;return a new state
(define state_new
  (lambda ()
    '((() ()))))

;acts as cdr for M_state
(define m_cdr
  (lambda (state)
    (list (cdar state) (cdr (cadr state)))))

;checks for an empty state
(define m_empty?
  (lambda (state)
    (null? state)))

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
          (else (l_lookup (car state) name)))))

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
    (list (cdar l) (cdadr l))))

;null? for layer
(define l_null?
  (lambda (l)
    (null? (car l))))

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
      ((equal? var (headvar l)) (headval l))
      (else (l_lookup (l_cdr l) var)))))

 ; return true if var is a member of the layer
(define l_member?
  (lambda (l var)
    (if (equal? 'undefined (l_lookup l var)) #t
    (#f))))
