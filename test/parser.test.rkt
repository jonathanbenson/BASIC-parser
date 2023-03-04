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

; test token-matcher function
(define check-token-matcher-x-valid
    '(
        (x "x")
        (EOL "\n")))

(define check-token-matcher-x-invalid
    '(
        (EOL "\n")))

(define token-matcher-x (token-matcher 'x))

(check-equal? (token-matcher-x check-token-matcher-x-valid) (list #t '((EOL "\n"))))
(check-equal? (token-matcher-x check-token-matcher-x-invalid) (list #f '((EOL "\n"))))

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

(define check-match-num-post-plus
    '(
        (PLUS "+")
        (NONZERO-DIGIT "1")
        (ZERO-DIGIT "0")
        (PLUS "+")
        (EOL "\n")))

(define check-match-num-post-minus
    '(
        (PLUS "+")
        (NONZERO-DIGIT "1")
        (ZERO-DIGIT "0")
        (MINUS "-")
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
(check-equal? (match-num check-match-num-post-plus) (list #t '((PLUS "+") (EOL "\n"))))
(check-equal? (match-num check-match-num-post-minus) (list #t '((MINUS "-") (EOL "\n"))))
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

(define check-match-expr-add
    '(
        (MINUS "-")
        (NONZERO-DIGIT "1")
        (ZERO-DIGIT "0")
        (PLUS "+")
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

(define check-match-expr-add-paren
    '(
        (LPAREN "(")
        (MINUS "-")
        (NONZERO-DIGIT "1")
        (ZERO-DIGIT "0")
        (PLUS "+")
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
(check-equal? (match-expr check-match-expr-add) (list #t '((EOL "\n"))))
(check-equal? (match-expr check-match-expr-paren) (list #t '((EOL "\n"))))
(check-equal? (match-expr check-match-expr-add-paren) (list #t '((EOL "\n"))))
(check-equal? (match-expr check-match-expr-missing-rparen) (list #f '((EOL "\n"))))


; test match-idx

(define check-match-idx-valid
    '(
        (NONZERO-DIGIT "1")
        (ZERO-DIGIT "0")
        (EOL "\n")))

(define check-match-idx-zero-digits
    '(
        (ZERO-DIGIT "0")
        (EOL "\n")))

(define check-match-idx-numsign
    '(
        (MINUS "-")
        (NONZERO-DIGIT "1")
        (ZERO-DIGIT "0")
        (EOL "\n")))

(define check-match-idx-invalid-token
    '(
        (LPAREN "(")
        (EOL "\n")))

(check-equal? (match-idx check-match-idx-valid) (list #t '((EOL "\n"))))
(check-equal? (match-idx check-match-idx-zero-digits) (list #f '((ZERO-DIGIT "0") (EOL "\n"))))
(check-equal? (match-idx check-match-idx-numsign) (list #f '((MINUS "-") (NONZERO-DIGIT "1") (ZERO-DIGIT "0") (EOL "\n"))))
(check-equal? (match-idx check-match-idx-invalid-token) (list #f '((LPAREN "(") (EOL "\n"))))

; test match-stmt

(define check-match-stmt-id
    '(
        (ID "x")
        (ASSIGN-OP "=")
        (NONZERO-DIGIT "5")
        (EOL "\n")))

(define check-match-stmt-if
    '(
        (IF "if")
        (NONZERO-DIGIT "5")
        (THEN "then")
        (RETURN "return")
        (EOL "\n")))

(define check-match-stmt-read
    '(
        (READ "read")
        (ID "x")
        (EOL "\n")))

(define check-match-stmt-write
    '(
        (WRITE "write")
        (NONZERO-DIGIT "1")
        (EOL "\n")))

(define check-match-stmt-goto
    '(
        (GOTO "goto")
        (NONZERO-DIGIT "1")
        (EOL "\n")))

(define check-match-stmt-gosub
    '(
        (GOSUB "gosub")
        (NONZERO-DIGIT "1")
        (EOL "\n")))

(define check-match-stmt-return
    '(
        (RETURN "return")
        (EOL "\n")))

(check-equal? (match-stmt check-match-stmt-id) (list #t '((EOL "\n"))))
(check-equal? (match-stmt check-match-stmt-if) (list #t '((EOL "\n"))))
(check-equal? (match-stmt check-match-stmt-read) (list #t '((EOL "\n"))))
(check-equal? (match-stmt check-match-stmt-write) (list #t '((EOL "\n"))))
(check-equal? (match-stmt check-match-stmt-goto) (list #t '((EOL "\n"))))
(check-equal? (match-stmt check-match-stmt-gosub) (list #t '((EOL "\n"))))
(check-equal? (match-stmt check-match-stmt-return) (list #t '((EOL "\n"))))