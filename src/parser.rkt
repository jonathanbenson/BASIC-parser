#lang racket

(require rackunit "../src/scanner.rkt")
(require rackunit "../src/token-matchers.rkt")

; input:
;   expected-token-type is a symbol representing the token type to check
;   token-stream is a list of tokens '((TOKEN-TYPE:symbol VAL:str) ...)
; output:
;   #t if the first token has the token type of token-type, else #f
(define (match-token expected-token-type token-stream)
    (if (eq? expected-token-type (first (first token-stream)))
        #t
        #f))

; input:
;   expected-token-types is a list of symbols representing the types of tokens to check
;   token-stream is a list of tokens '((TOKEN-TYPE:symbol VAL:str) ...)
; output:
;   #t if any of the expected-token-types match the token type of the first token in token-stream, else #f
(define (match-any-token expected-token-types token-stream)
    (cond
        [(empty? expected-token-types) #f]
        [(eq? (first expected-token-types) (first (first token-stream))) #t]
        [else (match-any-token (rest expected-token-types) token-stream)]))

(define (token-matcher token-type)
    (lambda (token-stream)
        (if (match-token token-type token-stream)
            (list #t (rest token-stream))
            (list #f token-stream))))

(define (match-many matchers token-stream)
    (if (empty? matchers)
        (list #t token-stream)
        (let ([match-result ((first matchers) token-stream)])
            (if (first match-result)
                (match-many (rest matchers) (second match-result))
                (list #f token-stream)))))

(define (match-num token-stream [found-numsign? #f] [found-digit? #f])
    (cond
        [(match-any-token '(PLUS MINUS) token-stream) (if (and found-numsign? found-digit?) (list #t token-stream) (match-num (rest token-stream) #t found-digit?))]
        [(match-any-token '(NONZERO-DIGIT ZERO-DIGIT) token-stream) (match-num (rest token-stream) found-numsign? #t)]
        [else (if found-digit? (list #t token-stream) (list #f token-stream))]))

(define (match-expr token-stream)
    (let ([match-num-result (match-num token-stream)])
        (cond
            [(match-token 'ID token-stream) (match-etail (rest token-stream))]
            [(first match-num-result) (match-etail (second match-num-result))]
            [else (match-many
                (list
                    (token-matcher 'LPAREN)
                    match-expr
                    (token-matcher 'RPAREN))
                token-stream)])))

(define (match-etail token-stream)
    (cond
        [(match-any-token '(PLUS MINUS ASSIGN-OP) token-stream) (match-expr (rest token-stream))]
        [else (list #t token-stream)]))

(define (match-idx token-stream [found-nonzero-digit? #f] [val ""])
    (cond
        [(match-token 'NONZERO-DIGIT token-stream) (match-idx (rest token-stream) #t (string-append val (second (first token-stream))))]
        [(match-token 'ZERO-DIGIT token-stream)
            (if found-nonzero-digit?
                (match-idx (rest token-stream) #t (string-append val (second (first token-stream))))
                (list #f token-stream))]
        [else (if found-nonzero-digit?
            (list #t token-stream val)
            (list #f token-stream))]))

(define (match-stmt token-stream)
    (let
        ([match-stmt-id-result
            (match-many (list (token-matcher 'ID) (token-matcher 'ASSIGN-OP) match-expr) token-stream)]
        [match-stmt-if-result
            (match-many (list (token-matcher 'IF) match-expr (token-matcher 'THEN)) token-stream)]
        [match-stmt-read-result
            (match-many (list (token-matcher 'READ) (token-matcher 'ID)) token-stream)]
        [match-stmt-write-result
            (match-many (list (token-matcher 'WRITE) match-expr) token-stream)]
        [match-stmt-goto-result
            (match-many (list (token-matcher 'GOTO) match-idx) token-stream)]
        [match-stmt-gosub-result
            (match-many (list (token-matcher 'GOSUB) match-idx) token-stream)]
        [match-stmt-return-result
            (match-many (list (token-matcher 'RETURN)) token-stream)])

        (cond
            [(first match-stmt-id-result) (list #t (second match-stmt-id-result))]
            [(first match-stmt-if-result) (match-stmt (second match-stmt-if-result))]
            [(first match-stmt-read-result) (list #t (second match-stmt-read-result))]
            [(first match-stmt-write-result) (list #t (second match-stmt-write-result))]
            [(first match-stmt-goto-result) (list #t (second match-stmt-goto-result))]
            [(first match-stmt-gosub-result) (list #t (second match-stmt-gosub-result))]
            [(first match-stmt-return-result) (list #t (second match-stmt-return-result))]
            [else (list #f token-stream)])))

(define (match-line token-stream)
    (let
        ([match-line-result
            (match-many (list match-idx match-stmt match-linetails (token-matcher 'EOL)) token-stream)]
        [match-idx-result (match-idx token-stream)])

        (if (first match-line-result)
            (list #t (second match-line-result) (if (first match-idx-result) (string->number (third match-idx-result)) 0))
            (list #f (second match-line-result) (if (first match-idx-result) (string->number (third match-idx-result)) 0)))))

(define (match-linetails token-stream)
    (let ([match-linetail-colon-result
        (match-many (list (token-matcher 'COLON) match-stmt) token-stream)])

        (if (first match-linetail-colon-result)
            (match-linetails (second match-linetail-colon-result))
            (list #t token-stream))))

(define (match-linelist token-stream)
    (let ([match-line-result (match-line token-stream)])
        (if (first match-line-result)
            (match-linelist (second match-line-result))
            (list #t (second match-line-result) (third match-line-result)))))

(define (match-program token-stream)
    (let ([match-linelist-result (match-linelist token-stream)])
        (if (and (first match-linelist-result) (match-token 'EOP (second match-linelist-result)))
            "Accept"
            (if (> (third match-linelist-result) 0)
                (string-append "Syntax error on line " (number->string (third match-linelist-result)))
                "Syntax error (no line number)"))))

(define (parse file-path)
    (match-program (scan (file->string file-path) token-matchers)))

(provide
    parse
    match-token
    match-any-token
    match-many
    token-matcher
    match-num
    match-expr
    match-idx
    match-stmt
    match-line
    parse)