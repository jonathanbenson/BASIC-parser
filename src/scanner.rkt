
#lang racket

(require "token-matchers.rkt")

; input:
;   char-stream is the file contents as a string
;   token-matchers is a list of functions that return
;       a list-pair (see return) of a token if the top of the
;       char stream matches to the token
; output:
;   list-pair '(TOKEN-TYPE TOKEN-VALUE)
(define (get-token char-stream token-matchers)
    (if (empty? token-matchers)

        ; if we have tried matching all the token types
        ; then there exists an invalid token at the top of the char-stream
        (list (list 'INVALID-TOKEN "x"))

        ; if a particular type of token exists at the top of the char stream
        ;   then return the token type and its value
        ; else try matching the next token
        (let ([match-result ((first token-matchers) char-stream)])
            (if (and (eq? match-result #f))
                (get-token char-stream (rest token-matchers))
                (list match-result)))))

; input:
;   char-stream is the file contents as a string
;   token-matchers is a list of functions that match tokens from a char stream
;   token-stream is the token stream that is built as scan is recursively called
; output:
;   a list of tokens (token stream) that are represented by list pairs of token type and value as symbols and strings respectively '(TOKEN-TYPE VALUE)
(define (scan char-stream token-matchers [token-stream '()])
  (let ([next-token (get-token char-stream token-matchers)])

    ; continue accepting tokens until the end of file
    (if (eq? (first (first next-token)) 'EOF)
        (append token-stream (list (first next-token)))

        ; ignore whitespace tokens
        (if (eq? (first (first next-token)) 'WS)
            (scan (substring char-stream (string-length (second (first next-token)))) token-matchers token-stream)

            ; if we have a match, add the new token to the token stream and try to match another token
            (scan (substring char-stream (string-length (second (first next-token)))) token-matchers (append token-stream next-token))))))

(provide
    get-token
    scan)