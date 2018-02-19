#lang racket
(require "simpleParser.scm")
;Interpreter EECS 345
;Aaron Weinberg and Johnathan Duffy

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
          ((eq? (caddr (M_Program (parser filename)(state_new) #f 0)) #t) 'TRUE)  ; if return_v is #t the return 'TRUE
          ((eq? (caddr (M_Program (parser filename)(state_new) #f 0)) #f) 'FALSE) ; if return_v is #f the return 'FALSE
          (else (caddr (M_Program (parser filename)(state_new) #f 0))))         ; else return return_v which will be an integer
        (error "No Return Value")))) ;if return_b is false then throw error        

;return a new state
(define state_new
  (lambda ()
    '(() ())))

;M_Program calls M_Forward_OP on the next expr until there returb_b is true
;reutrns (state return_b return_v)
;
;TODO finish, if return_b is ture of the program is empty then return the state and value
;else run M_Forward_OP on the next expr in the program
(define M_Program
  (lambda (program state return_b return_v)
    (cons state (cons (cadr (M_Forward_OP (car program) state return_b return_v)) (cons (caddr (M_Forward_OP (car program) state return_b return_v)) '())))
    ))


;there wasn't a != operator, so now there is.
(define !=
  (lambda (x y)
    (not (= x y))))


;Takes a single operation in form of list (operation args1 args2 etc...) ard forwards the list to the correct operation
(define M_Forward_OP 
  (lambda (expr state return_b return_v)
    (cond
      ((eq? return_b #t) (cons state (cons return_b (cons return_v '())))) ;if return_b is true then return state and values
      ((eq? (car expr) 'var)      (declaration_OP expr))
      ((eq? (car expr) '=)        (assignment_OP expr))
      ((eq? (car expr) 'return)   (cons state (cons #t (cons (cadr (return_OP expr state)) '()))))
      ((eq? (car expr) 'if)       (if_OP expr))
      ((eq? (car expr) 'while)    (while_OP expr))
      (else (error "Invalid Expression: " expr)))))

;Declaration (var variable (value optional))
(define declaration_OP
  (lambda (lis)
    (car lis)))

;Assignment (= variable expression)
(define assignment_OP
  (lambda (lis)
    (car lis)))

;Return (return expression)
(define return_OP
  (lambda (expr state)
    (cons state (cons (arith_eval (cadr expr)) '()))))

;if statement (if conditional then-statement optional-else-statement)
(define if_OP
  (lambda (lis)
    (car lis)))

;while statement (while conditional body-statement)
(define while_OP
  (lambda (lis)
    (car lis)))

	
;arith_eval - Function that takes a simple or compound arithmetic expression (* + - / %) and returns the proper return value. (Doesn't take variables yet)
(define arith_eval
  (lambda (expr)
    (cond ((number? expr) expr);if single number, return it.
    (else (let ((op (car expr));else, take arguments, handle compound expressions if necessary
                (arg1 (arith_eval (cadr expr)))
                (arg2 (arith_eval (caddr expr))))
            (cond ((eq? op '*) ;snarf underlying Scheme operators.
                   (* arg1 arg2))
                  ((eq? op '+)
                   (+ arg1 arg2))
                  ((eq? op '-)
                   (- arg1 arg2))
                  ((eq? op '/)
                   (/ arg1 arg2))
                  ((eq? op '%)
                   (remainder arg1 arg2))                  
                  (else
                   (error "Invalid operation in: " expr)))))))) ;throw error if operator isn't one of those.

;list_index - Takes a list and a symbol, returns index of that symbol
(define list_index
  (lambda (lst sym)
    (cond ((null? lst) -1)
          ((eq? (car lst) sym) 0)
          ((= (list_index (cdr lst) sym) -1) -1)
          (else (+ 1 (list_index (cdr lst) sym))))))


;Mstate stuff -----------------------------------------------------

;get first variable in the state
(define state_head_var caar)

;get value of first variable in state
(define state_head_val caadr)

; Function that binds a name and value pair to a state
(define state_bind
  (lambda (state name value)
    (list (cons name (car state)) (cons value (cadr state)))))


;Mvalue stuff -----------------------------------------------------


          
           