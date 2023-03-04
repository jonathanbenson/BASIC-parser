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

; test match-any
(check-equal? (match-any-token (list 'NONZERO-DIGIT 'ZERO-DIGIT 'GOTO) check-token-stream) #t)
(check-equal? (match-any-token (list 'ZERO-DIGIT 'GOTO 'NONZERO-DIGIT) check-token-stream) #t)
(check-equal? (match-any-token (list 'ZERO-DIGIT 'GOTO) check-token-stream) #f)

; test match-many
(define match-many-abc-matchers
    (list
        (lambda (token-stream)
            (if (match-token 'A token-stream)
                (list #t (rest token-stream))
                (list #f (rest token-stream))))
        (lambda (token-stream)
            (if (match-token 'B token-stream)
                (list #t (rest token-stream))
                (list #f (rest token-stream))))
        (lambda (token-stream)
            (if (match-token 'C token-stream)
                (list #t (rest token-stream))
                (list #f (rest token-stream))))))

(define match-many-abc-token-stream
    '(
        (A "A")
        (B "B")
        (C "C")
        (EOL "\n")))

(check-equal? (match-many match-many-abc-matchers match-many-abc-token-stream)
    (list #t '((EOL "\n"))))

; test syntax-error function

(check-equal? (syntax-error 1 "x" 'x) "Syntax error on line 1. Debug message: x... Last token: x")
(check-equal? (syntax-error 5 "x" 'x) "Syntax error on line 5. Debug message: x... Last token: x")

; test match-num function
(define check-match-num-no-numsign
    '(
        (NONZERO-DIGIT "1")
        (ZERO-DIGIT "0")
        (EOL "\n")))

(define check-match-num-minus
    '(
        (MINUS "-")
        (NONZERO-DIGIT "1")
        (ZERO-DIGIT "0")
        (EOL "\n")))

(define check-match-num-plus
    '(
        (PLUS "+")
        (NONZERO-DIGIT "1")
        (ZERO-DIGIT "0")
        (EOL "\n")))

(define check-match-num-plus-zeros
    '(
        (PLUS "+")
        (ZERO-DIGIT "0")
        (EOL "\n")))

(define check-match-num-minus-zeros
    '(
        (MINUS "-")
        (ZERO-DIGIT "0")
        (EOL "\n")))

(define check-match-num-minus-only
    '(
        (MINUS "-")
        (EOL "\n")))

(define check-match-num-plus-only
    '(
        (PLUS "+")
        (EOL "\n")))

(define check-match-num-invalid-token
    '(
        (EOL "\n")))

(check-equal? (match-num check-match-num-no-numsign) (list #t '((EOL "\n"))))
(check-equal? (match-num check-match-num-minus) (list #t '((EOL "\n"))))
(check-equal? (match-num check-match-num-plus) (list #t '((EOL "\n"))))
(check-equal? (match-num check-match-num-minus-zeros) (list #t '((EOL "\n"))))
(check-equal? (match-num check-match-num-plus-zeros) (list #t '((EOL "\n"))))
(check-equal? (match-num check-match-num-minus-only) (list #f '((EOL "\n"))))
(check-equal? (match-num check-match-num-plus-only) (list #f '((EOL "\n"))))
(check-equal? (match-num check-match-num-invalid-token) (list #f '((EOL "\n"))))

; test match-expr function
(define check-match-expr-id
    '(
        (ID "asdf")
        (EOL "\n")))

(define check-match-expr-num
    '(
        (MINUS "-")
        (NONZERO-DIGIT "1")
        (ZERO-DIGIT "0")
        (EOL "\n")))

(define check-match-expr-paren
    '(
        (LPAREN "(")
        (MINUS "-")
        (NONZERO-DIGIT "1")
        (ZERO-DIGIT "0")
        (RPAREN ")")
        (EOL "\n")))

(define check-match-expr-missing-rparen
    '(
        (LPAREN "(")
        (NONZERO-DIGIT "1")
        (ZERO-DIGIT "0")
        (EOL "\n")))

(check-equal? (match-expr check-match-expr-id) (list #t '((EOL "\n"))))
(check-equal? (match-expr check-match-expr-num) (list #t '((EOL "\n"))))
(check-equal? (match-expr check-match-expr-paren) (list #t '((EOL "\n"))))
(check-equal? (match-expr check-match-expr-missing-rparen) (list #f '((EOL "\n"))))