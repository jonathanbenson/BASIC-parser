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
        ;;; [(match-any-token (list 'ID 'IF 'READ 'WRITE 'GOTO 'GOSUB 'RETURN)) (stmt token-stream line-number)]
        ;;; [(match-token 'COLON token-stream) (linetail token-stream line-number)]
        [(match-token 'EOL token-stream) (linelist token-stream (+ line-number 1))]
        [else (syntax-error line-number)]))

(define (idx token-stream line-number parent-callback)
    (cond
        [(match-token 'NONZERO-DIGIT token-stream) (idx (rest token-stream) line-number)]
        [else (parent-callback (rest token-stream) line-number)]))

;;; (define (expr token-stream line-number parent-callback)
;;;     )

;;; (define (stmt token-stream line-number)
;;;     (cond
;;;         [(and (match-token 'READ token-stream) (match-token 'ID (rest token-stream))) (linetail (rest (rest token-stream)) line-number)]
;;;         [(and (match-token 'READ token-stream) (match-token 'ID (rest token-stream))) (linetail (rest (rest token-stream)) line-number)]))

(provide
    match-token
    match-any-token
    syntax-error
    program)