
#lang racket

(define nonzero-digits (list #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

; the following functions check to see whether or not the start of the char-stream
; starts with a particular pattern
; If true, then it returns a list with the first element a symbol of the token type
; else it returns false
(define (eop? char-stream)
    (if (string-prefix? char-stream "$$")
        (list 'EOP "$$")
        #f))

(define (eol? char-stream)
    (if (string-prefix? char-stream "\n")
        (list 'EOL "\n")
        #f))

; an idx is zero or more nonzero digits
(define (idx? char-stream [idx ""])
    
    ; if the char-stream is not empty and the first char in the char-stream is a nonzero digit
    (if (and (non-empty-string? char-stream) (not (eq? (member (string-ref char-stream 0) nonzero-digits) #f)))
        (idx? (substring char-stream 1) (string-append idx (substring char-stream 0 1)))

        (if (not (non-empty-string? idx))
            #f
            (list 'IDX idx))))

(define (colon? char-stream)
    (if (string-prefix? char-stream ":")
        (list 'COLON ":")
        #f))

(define (id? char-stream [id ""])
    ; if the char-stream is not empty and the first char in the char-stream is a letter (upper or lowercase)
    (if (and (non-empty-string? char-stream) (char-alphabetic? (string-ref char-stream 0)))
        (id? (substring char-stream 1) (string-append id (substring char-stream 0 1)))

        (if (not (non-empty-string? id))
            #f
            (list 'ID id))))

(define (assign-op? char-stream)
    (if (string-prefix? char-stream "=")
        (list 'ASSIGN-OP "=")
        #f))

(define (if? char-stream)
    (if (string-prefix? char-stream "if")
        (list 'IF "if")
        #f))

(define (then? char-stream)
    (if (string-prefix? char-stream "then")
        (list 'THEN "then")
        #f))

(define (read? char-stream)
    (if (string-prefix? char-stream "read")
        (list 'READ "read")
        #f))

(define (write? char-stream)
    (if (string-prefix? char-stream "write")
        (list 'WRITE "write")
        #f))

(define (goto? char-stream)
    (if (string-prefix? char-stream "goto")
        (list 'GOTO "goto")
        #f))

(define (gosub? char-stream)
    (if (string-prefix? char-stream "gosub")
        (list 'GOSUB "gosub")
        #f))

(define (return? char-stream)
    (if (string-prefix? char-stream "return")
        (list 'RETURN "return")
        #f))

(define (lparen? char-stream)
    (if (string-prefix? char-stream "(")
        (list 'LPAREN "(")
        #f))

(define (rparen? char-stream)
    (if (string-prefix? char-stream ")")
        (list 'RPAREN ")")
        #f))

(define (plus? char-stream)
    (if (string-prefix? char-stream "+")
        (list 'PLUS "+")
        #f))

(define (minus? char-stream)
    (if (string-prefix? char-stream "-")
        (list 'MINUS "-")
        #f))

(define (num? char-stream)
    (if (string-prefix? char-stream "return")
        (list 'RETURN "return")
        #f))

(define (numsign? char-stream)
    (if (or (not (eq? (plus? char-stream) #f) (or (not (eq? (minus? char-stream))))))
        (list 'RETURN "return")
        #f))

(provide
    eop?
    eol?
    idx?
    colon?
    assign-op?
    if?
    then?
    read?
    write?
    goto?
    gosub?
    return?
    lparen?
    rparen?
    id?)