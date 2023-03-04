#lang racket

(require rackunit "../src/scanner.rkt")
(require rackunit "../src/token-matchers.rkt")

;;; (define (parse file-path)
;;;     (program (scan (file->string file-path) token-matchers)))

(define (match-token expected-token-type token-stream)
    (if (eq? expected-token-type (first (first token-stream)))
        #t
        #f))

(define (match-any-token expected-token-types token-stream)
    (cond
        [(empty? expected-token-types) #f]
        [(eq? (first expected-token-types) (first (first token-stream))) #t]
        [else (match-any-token (rest expected-token-types) token-stream)]))

(define (syntax-error line-number debug-msg last-token)
    (string-append "Syntax error on line " (number->string line-number) ". Debug message: " debug-msg "... Last token: " (symbol->string last-token)))

(define (match-many-nonterminals nonterminal-matchers token-stream)
    (if (empty? nonterminal-matchers)
        (list #t token-stream)
        (let ([match-result ((first nonterminal-matchers) token-stream)])
            (if (first match-result)
                (match-many-nonterminals (rest nonterminal-matchers) (second match-result))
                (list #f token-stream)))))

(define (match-num token-stream [found-numsign? #f] [found-digit? #f])
    (cond
        [(empty? token-stream) (if found-digit? (list #t token-stream) (list #f token-stream))]
        [(match-any-token '(PLUS MINUS) token-stream) (if found-digit? (list #f (rest token-stream)) (match-num (rest token-stream) #t found-digit?))]
        [(match-any-token '(NONZERO-DIGIT ZERO-DIGIT) token-stream) (match-num (rest token-stream) found-numsign? #t)]
        [else (if found-digit? (list #t token-stream) (list #f token-stream))]))


(provide
    ;;; parse
    match-token
    match-any-token
    syntax-error
    match-num)