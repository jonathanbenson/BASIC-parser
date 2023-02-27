#lang racket

(require rackunit "../src/parser.rkt")

; test match function

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


