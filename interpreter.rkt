#lang racket
(require "simpleParser.scm")

;Main Function that takes a filename calles parser on it, evaluates the parse tree and returns the proper value (or error if a variable is used before declared).
;call (interpret "TestCode.txt")
(define interpret
  (lambda (filename)
    ((parser filename))))

;Environment declaration that will maintain the interpreter's environment, begins as two lists; one for
;variable names, the second for the corresponding values.
(define env '(('return) (null)))

;arith_eval - Function that takes a simple or compound arithmetic expression (* + - /) and returns the proper return value.
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
                  
          
           