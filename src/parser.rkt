#lang racket

(define (match-token expected-token-type token-stream)
    (if (eq? expected-token-type (first (first token-stream)))
        #t
        #f))

(define (match-any-token expected-token-types token-stream)
    (cond
        [(empty? expected-token-types) #f]
        [(eq? (first expected-token-types) (first (first token-stream))) #t]
        [else (match-any-token (rest expected-token-types) token-stream)]))

(define (syntax-error line-number)
    (string-append "Syntax error on line " (number->string line-number) "."))

(define (program token-stream [line-number 1])
    (cond
        [(match-token 'NONZERO-DIGIT token-stream) (linelist (rest token-stream) line-number)]
        [(match-token 'EOP token-stream) "Accept"]
        [else (syntax-error line-number)]))

(define (linelist token-stream line-number)
    (cond
        [(match-token 'NONZERO-DIGIT token-stream) (line (rest token-stream) line-number)]
        [(match-token 'EOP token-stream) (program token-stream line-number)]
        [else (syntax-error line-number)]))

(define (line token-stream line-number)
    (cond
        [(match-token 'NONZERO-DIGIT token-stream) (idx (rest token-stream) line-number line)]
        [(match-any-token (list 'ID 'IF 'READ 'WRITE 'GOTO 'GOSUB 'RETURN)) (stmt token-stream line-number)]
        [(match-token 'COLON token-stream) (linetail token-stream line-number)]
        [(match-token 'EOL token-stream) (linelist token-stream (+ line-number 1))]
        [else (syntax-error line-number)]))

(define (idx token-stream line-number parent-callback)
    (cond
        [(match-token 'NONZERO-DIGIT token-stream) (idx (rest token-stream) line-number)]
        [else (parent-callback (rest token-stream) line-number)]))

(define (stmt token-stream line-number)
    (cond
        [(and (match-token 'ID token-stream) (match-token 'ASSIGN-OP (rest token-stream))) (expr (rest (rest token-stream)) line-number linetail)]
        [(match-token 'IF token-stream) (expr (rest token-stream) line-number stmt)]
        [(match-token 'THEN token-stream) (stmt (rest token-stream) line-number)]
        [(and (match-token 'READ token-stream) (match-token 'ID (rest token-stream))) (linetail (rest (rest token-stream)) line-number)]
        [(match-token 'WRITE token-stream) (expr (rest token-stream) line-number linetail)]
        [(match-token 'GOTO token-stream) (idx (rest token-stream) line-number linetail)]
        [(match-token 'GOSUB token-stream) (idx (rest token-stream) line-number linetail)]
        [(match-token 'RETURN token-stream) (linetail (rest token-stream) line-number)]))

(define (linetail token-stream line-number)
    (cond
        [(match-token 'COLON token-stream) (stmt (rest token-stream line-number))]
        [else (line token-stream line-number)]))

(define (expr token-stream line-number parent-callback)
    (cond
        [(match-token 'ID token-stream) (etail (rest token-stream) line-number)]
        [(match-token 'LPAREN token-stream) (expr (rest token-stream) line-number expr)]
        [else (num token-stream line-number expr)]))

(define (etail token-stream line-number)
    (cond
        [(match-any-token (list 'PLUS 'MINUS 'ASSIGN-OP) (expr (rest token-stream line-number)))]
        [else (syntax-error line-number)]))

(define (num token-stream line-number parent-callback)
    (cond
        [(match-any-token (list 'PLUS 'MINUS) token-stream) (num (rest token-stream) line-number parent-callback)]
        [(match-any-token (list 'ZERO-DIGIT 'NONZERO-DIGIT) token-stream) (num (rest token-stream) line-number parent-callback)]
        [else (parent-callback token-stream line-number)]))

(provide
    match-token
    match-any-token
    syntax-error
    program)