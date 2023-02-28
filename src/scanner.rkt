
#lang racket

(require "token-matchers.rkt")

(define (get-token char-stream token-matchers)
  (if (empty? token-matchers)
      (list (list 'INVALID-TOKEN "x"))
      (let ([match-result ((first token-matchers) char-stream)])
        (if (and (eq? match-result #f))
            (get-token char-stream (rest token-matchers))
            (list match-result)))))

(define (scan char-stream token-matchers [token-stream '()])
  (let ([next-token (get-token char-stream token-matchers)])
    (if (eq? (first (first next-token)) 'EOF)
        (append token-stream (list (first next-token)))
        (if (eq? (first (first next-token)) 'WS)
            (scan (substring char-stream (string-length (second (first next-token)))) token-matchers token-stream)
            (scan (substring char-stream (string-length (second (first next-token)))) token-matchers (append token-stream next-token))))))

(provide
    get-token
    scan)