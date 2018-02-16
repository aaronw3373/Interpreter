#lang racket
(require "simpleParser.scm")

;Main Function that takes a filename calles parser on it, evaluates the parse tree and returns the proper value (or error if a variable is used before declared).
;call (interpret "TestCode.txt")
(define interpret
  (lambda (filename)
    ((parser filename))))

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
                  
          
           