#lang racket
(require "simpleParser.scm")

;Main Function that takes a filename calles parser on it, evaluates the parse tree and returns the proper value (or error if a variable is used before declared).
;call (interpret "TestCode.txt")
(define interpret
  (lambda (filename)
    (arith_eval (parser filename))))

;arith_eval - Function that takes a simple or compound arithmetic expression (* + - /) and returns the proper return value.
(define arith_eval
  (lambda (expr)
    (cond ((number? expr) expr);if single number, return it.
    (else (let ((op (car expr))
                (arg1 (arith_eval (cadr expr)))
                (arg2 (arith_eval (caddr expr))))
            (cond ((eq? op '*)
                   (* arg1 arg2))
                  ((eq? op '+)
                   (+ arg1 arg2))
                  ((eq? op '-)
                   (- arg1 arg2))
                  ((eq? op '/)
                   (/ arg1 arg2))
                  (else
                   (error "Invalid operation in: " expr))))))))
                  
          
           