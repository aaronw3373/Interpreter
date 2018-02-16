#lang racket
(require "simpleParser.scm")

;atom? function to tell if something is an atom
(define (atom? x) (not (or (pair? x) (null? x) (vector? x))))

;Main Function that takes a filename calles parser on it, evaluates the parse tree and returns the proper value (or error if a variable is used before declared).
;call (interpret "TestCode.txt")
(define interpret
  (lambda (filename)
    (Forward_OP (car (parser filename)))));Test first expression for now

;Takes a single operation in form of list (operation args1 args2 etc...) ard forwards the list to the correct operation
(define Forward_OP 
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((eq? (car lis) list?) (Forward_OP (car lis)))
      ((atom? (car lis))  (cond
                             ((eq? (car lis) 'var)      (declaration_OP lis))
                             ((eq? (car lis) '=)        (assignment_OP lis))
                             ((eq? (car lis) 'return)   (return_OP lis))
                             ((eq? (car lis) 'if)       (if_OP lis))
                             ((eq? (car lis) 'while)    (while_OP lis))))
      (else (error "Invalid single operation in: " lis)))))

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
  (lambda (lis)
    (arith_eval (cadr lis))))

;if statement (if conditional then-statement optional-else-statement)
(define if_OP
  (lambda (lis)
    (car lis)))

;while statement (while conditional body-statement)
(define while_OP
  (lambda (lis)
    (car lis)))

<<<<<<< HEAD
;arith_eval - Function that takes a simple or compound arithmetic expression (* + - / %) and returns the proper return value. (Doesn't take variables yet)
=======
;Environment declaration that will maintain the interpreter's environment, begins as two lists; one for
;variable names, the second for the corresponding values.
(define env '(('return) (null)))

;arith_eval - Function that takes a simple or compound arithmetic expression (* + - /) and returns the proper return value.
>>>>>>> 81a56a6069d581ebc3da9dd2b175cdb4f2de3e20
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

;bind_var - Function that takes a name and value, and binds them to the interpreter's environment.
;(define bind_var
 ; (lambda (name value)
  ;    (if (member? name (car env)) ;if binding already exists
   ;       ( ))))

;list_index - Takes a list and a symbol, returns index of that symbol
(define list_index
  (lambda (lst sym)
    (cond ((null? lst) -1)
          ((eq? (car lst) sym) 0)
          ((= (list_index (cdr lst) sym) -1) -1)
          (else (+ 1 (list_index (cdr lst) sym))))))
                  
          
           