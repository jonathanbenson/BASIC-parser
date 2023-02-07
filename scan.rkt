
#lang racket

(define whitespaces (list #\space #\tab))
(define nonzero-digits (list #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

; Possible token types:
; EOP -> "$$"
; EOL -> "\n"
; ZERO-DIGIT -> "0"
; NONZERO-DIGIT -> "1"-"9"
; IF -> "if"
; READ -> "read"
; WRITE -> "write"
; GOTO -> "goto"
; GOSUB -> "gosub"
; ID -> [a-zA-Z]
; COLON -> ":"
; THEN -> "then"
; ASSIGN-OP -> "="
; PLUS -> "+"
; MINUS -> "-"
; LPAREN -> "("
; RPAREN -> ")"
; RETURN -> "return"
; WS -> whitespace

; the following functions check to see whether or not the start of the char-stream
; starts with a particular pattern
; If true, then it returns a list with the first element a symbol of the token type
; else it returns false


; EOP -> "$$"
(define (eop? char-stream)
    (if (string-prefix? char-stream "$$")
        (list 'EOP "$$")
        #f))

; EOL -> "\n"
(define (eol? char-stream)
    (if (string-prefix? char-stream "\n")
        (list 'EOL "\n")
        #f))

; ZERO-DIGIT -> "0"
(define (zero-digit? char-stream)
    (if (eq? (string-ref char-stream 0) #\0)
        (list 'ZERO-DIGIT "0")
        #f))

; NONZERO-DIGIT -> "1"-"9"
(define (nonzero-digit? char-stream)
    (if (member (string-ref char-stream 0) nonzero-digits)
        (list 'NONZERO-DIGIT (string (string-ref char-stream 0)))
        #f))

; IF -> "if"
(define (if? char-stream)
    (if (string-prefix? char-stream "if")
        (list 'IF "if")
        #f))

; READ -> "read"
(define (read? char-stream)
    (if (string-prefix? char-stream "read")
        (list 'READ "read")
        #f))

; WRITE -> "write"
(define (write? char-stream)
    (if (string-prefix? char-stream "write")
        (list 'WRITE "write")
        #f))

; GOTO -> "goto"
(define (goto? char-stream)
    (if (string-prefix? char-stream "goto")
        (list 'GOTO "goto")
        #f))

; GOSUB -> "gosub"
(define (gosub? char-stream)
    (if (string-prefix? char-stream "gosub")
        (list 'GOSUB "gosub")
        #f))

; ID -> [a-zA-Z]
(define (id? char-stream [id ""])
    ; if the char-stream is not empty and the first char in the char-stream is a letter (upper or lowercase)
    (if (and (non-empty-string? char-stream) (char-alphabetic? (string-ref char-stream 0)))
        (id? (substring char-stream 1) (string-append id (substring char-stream 0 1)))

        (if (not (non-empty-string? id))
            #f
            (list 'ID id))))

; COLON -> ":"
(define (colon? char-stream)
    (if (string-prefix? char-stream ":")
        (list 'COLON ":")
        #f))

; THEN -> "then"
(define (then? char-stream)
    (if (string-prefix? char-stream "then")
        (list 'THEN "then")
        #f))

; ASSIGN-OP -> "="
(define (assign-op? char-stream)
    (if (string-prefix? char-stream "=")
        (list 'ASSIGN-OP "=")
        #f))

; PLUS -> "+"
(define (plus? char-stream)
    (if (string-prefix? char-stream "+")
        (list 'PLUS "+")
        #f))

; MINUS -> "-"
(define (minus? char-stream)
    (if (string-prefix? char-stream "-")
        (list 'MINUS "-")
        #f))

; LPAREN -> "("
(define (lparen? char-stream)
    (if (string-prefix? char-stream "(")
        (list 'LPAREN "(")
        #f))

; RPAREN -> ")"
(define (rparen? char-stream)
    (if (string-prefix? char-stream ")")
        (list 'RPAREN ")")
        #f))

; RETURN -> "return"
(define (return? char-stream)
    (if (string-prefix? char-stream "return")
        (list 'RETURN "return")
        #f))

; WS -> whitespace
(define (ws? char-stream [ws ""])
    
    ; if the char-stream is not empty and the first char in the char-stream is a whitespace char
    (if (and (non-empty-string? char-stream) (not (eq? (member (string-ref char-stream 0) whitespaces) #f)))
        (ws? (substring char-stream 1) (string-append ws (substring char-stream 0 1)))

        (if (not (non-empty-string? ws))
            #f
            (list 'WS ws))))

(provide
    ws?
    eop?
    eol?
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
    id?
    zero-digit?
    nonzero-digit?)