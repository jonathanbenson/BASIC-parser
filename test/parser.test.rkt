#lang racket

(require rackunit "../src/parser.rkt")

; test match-token function

(define check-token-stream
    '(
        (NONZERO-DIGIT "1")
        (ZERO-DIGIT "0")
        (GOTO "goto")
        (NONZERO-DIGIT "2")
        (ZERO-DIGIT "0")
        (EOL "\n")
        (NONZERO-DIGIT "2")
        (ZERO-DIGIT "0")
        (GOTO "goto")
        (NONZERO-DIGIT "1")
        (ZERO-DIGIT "0")
        (EOL "\n")
        (EOP "$$")
        (EOL "\n")
        (EOF "")))

(check-equal? (match-token 'NONZERO-DIGIT check-token-stream) #t)
(check-equal? (match-token 'ZERO-DIGIT check-token-stream) #f)

; test match-any-token
(check-equal? (match-any-token (list 'NONZERO-DIGIT 'ZERO-DIGIT 'GOTO) check-token-stream) #t)
(check-equal? (match-any-token (list 'ZERO-DIGIT 'GOTO 'NONZERO-DIGIT) check-token-stream) #t)
(check-equal? (match-any-token (list 'ZERO-DIGIT 'GOTO) check-token-stream) #f)

; test syntax-error function

(check-equal? (syntax-error 1 "x" 'x) "Syntax error on line 1. Debug message: x... Last token: x")
(check-equal? (syntax-error 5 "x" 'x) "Syntax error on line 5. Debug message: x... Last token: x")

; test program function
(check-equal? (program '((EOP "$$"))) "Accept")
(check-equal? (program '((ZERO-DIGIT "0"))) "Syntax error on line 1. Debug message: program... Last token: ZERO-DIGIT")