
#lang racket

(require rackunit "../src/token-matchers.rkt")
(require rackunit "../src/scanner.rkt")

; scan
(define file01-char-stream (file->string "test_input/file01.txt"))
(define file02-char-stream (file->string "test_input/file02.txt"))
(define file03-char-stream (file->string "test_input/file03.txt"))
(define file04-char-stream (file->string "test_input/file04.txt"))
(define file05-char-stream (file->string "test_input/file05.txt"))
(define file06-char-stream (file->string "test_input/file06.txt"))

; scan test over file01
(check-equal? (scan file01-char-stream token-matchers)
    (list
        'NONZERO-DIGIT "1"
        'ZERO-DIGIT "0"
        'GOTO "goto"
        'NONZERO-DIGIT "2"
        'ZERO-DIGIT "0"
        'EOL "\n"
        'NONZERO-DIGIT "2"
        'ZERO-DIGIT "0"
        'GOTO "goto"
        'NONZERO-DIGIT "1"
        'ZERO-DIGIT "0"
        'EOL "\n"
        'EOP "$$"
        'EOL "\n"
        'EOF ""))

; scan test over file02
(check-equal? (scan file02-char-stream token-matchers)
    (list
        'NONZERO-DIGIT "1"
        'ZERO-DIGIT "0"
        'ID "A"
        'ASSIGN-OP "="
        'NONZERO-DIGIT "1"
        'ZERO-DIGIT "0"
        'EOL "\n"

        'NONZERO-DIGIT "2"
        'ZERO-DIGIT "0"
        'ID "B"
        'ASSIGN-OP "="
        'NONZERO-DIGIT "2"
        'ZERO-DIGIT "0"
        'EOL "\n"

        'NONZERO-DIGIT "3"
        'ZERO-DIGIT "0"
        'ID "C"
        'ASSIGN-OP "="
        'ID "B"
        'INVALID-TOKEN "x"
        'ID "A"
        'EOL "\n"

        'NONZERO-DIGIT "4"
        'ZERO-DIGIT "0"
        'WRITE "write"
        'ID "C"
        'EOL "\n"

        'EOP "$$"
        'EOL "\n"
        'EOF ""))



; scan test for file03
(check-equal? (scan file03-char-stream token-matchers)
    (list
        'NONZERO-DIGIT "1"
        'ZERO-DIGIT "0"
        'READ "read"
        'ID "A"
        'EOL "\n"
        'NONZERO-DIGIT "2"
        'ZERO-DIGIT "0"
        'READ "read"
        'ID "B"
        'EOL "\n"
        'NONZERO-DIGIT "3"
        'ZERO-DIGIT "0"
        'GOSUB "gosub"
        'NONZERO-DIGIT "4"
        'ZERO-DIGIT "0"
        'ZERO-DIGIT "0"
        'EOL "\n"
        'NONZERO-DIGIT "4"
        'ZERO-DIGIT "0"
        'IF "if"
        'ID "C"
        'ASSIGN-OP "="
        'NONZERO-DIGIT "4"
        'ZERO-DIGIT "0"
        'ZERO-DIGIT "0"
        'THEN "then"
        'WRITE "write"
        'ID "C"
        'EOL "\n"
        'NONZERO-DIGIT "5"
        'ZERO-DIGIT "0"
        'IF "if"
        'ID "C"
        'ASSIGN-OP "="
        'ZERO-DIGIT "0"
        'THEN "then"
        'GOTO "goto"
        'NONZERO-DIGIT "1"
        'ZERO-DIGIT "0"
        'ZERO-DIGIT "0"
        'ZERO-DIGIT "0"
        'EOL "\n"
        'NONZERO-DIGIT "4"
        'ZERO-DIGIT "0"
        'ZERO-DIGIT "0"
        'ID "C"
        'ASSIGN-OP "="
        'ID "A"
        'PLUS "+"
        'ID "B"
        'COLON ":"
        'RETURN "return"
        'EOL "\n"
        'EOP "$$"
        'EOL "\n"
        'EOF ""))

        
; scan test for file06
(check-equal? (scan file06-char-stream token-matchers)
    (list
        'NONZERO-DIGIT "1"
        'ZERO-DIGIT "0"
        'ZERO-DIGIT "0"
        'READ "read"
        'ID "A"
        'EOL "\n"
        'NONZERO-DIGIT "2"
        'ZERO-DIGIT "0"
        'ZERO-DIGIT "0"
        'READ "read"
        'ID "B"
        'EOL "\n"
        'NONZERO-DIGIT "3"
        'ZERO-DIGIT "0"
        'ZERO-DIGIT "0"
        'ZERO-DIGIT "0"
        'ZERO-DIGIT "0"
        'WRITE "write"
        'ID "A"
        'PLUS "+"
        'LPAREN "("
        'ID "B"
        'MINUS "-"
        'NONZERO-DIGIT "1"
        'RPAREN ")"
        'EOL "\n"
        'EOP "$$"
        'EOL "\n"
        'EOF ""))


; get-token

; get-token ... EOF -> ""
(check-equal? (first (get-token "" token-matchers)) 'EOF)

; get-token ... EOP -> "$$"
(check-equal? (first (get-token "$$" token-matchers)) 'EOP)
(check-equal? (first (get-token "$$\n\n" token-matchers)) 'EOP)
(check-equal? (first (get-token "$$abc" token-matchers)) 'EOP)
(check-equal? (first (get-token "$$123" token-matchers)) 'EOP)

; get-token ... EOL -> "\n"
(check-equal? (first (get-token "\n" token-matchers)) 'EOL)
(check-equal? (first (get-token "\n\n\n" token-matchers)) 'EOL)
(check-equal? (first (get-token "\nabc" token-matchers)) 'EOL)
(check-equal? (first (get-token "\n123" token-matchers)) 'EOL)

; get-token ... ZERO-DIGIT -> "0"
(check-equal? (first (get-token "0" token-matchers)) 'ZERO-DIGIT)
(check-equal? (first (get-token "0\n\n" token-matchers)) 'ZERO-DIGIT)
(check-equal? (first (get-token "0abc" token-matchers)) 'ZERO-DIGIT)
(check-equal? (first (get-token "0123" token-matchers)) 'ZERO-DIGIT)

; get-token ... NONZERO-DIGIT -> "1"-"9"
(check-equal? (first (get-token "1" token-matchers)) 'NONZERO-DIGIT)
(check-equal? (first (get-token "1\n\n" token-matchers)) 'NONZERO-DIGIT)
(check-equal? (first (get-token "1abc" token-matchers)) 'NONZERO-DIGIT)
(check-equal? (first (get-token "1123" token-matchers)) 'NONZERO-DIGIT)

; get-token ... IF -> "if"
(check-equal? (first (get-token "if" token-matchers)) 'IF)
(check-equal? (first (get-token "if\n\n" token-matchers)) 'IF)
(check-equal? (first (get-token "ifabc" token-matchers)) 'IF)
(check-equal? (first (get-token "if123" token-matchers)) 'IF)

; get-token ... READ -> "read"
(check-equal? (first (get-token "read" token-matchers)) 'READ)
(check-equal? (first (get-token "read\n\n" token-matchers)) 'READ)
(check-equal? (first (get-token "readabc" token-matchers)) 'READ)
(check-equal? (first (get-token "read123" token-matchers)) 'READ)

; get-token ... WRITE -> "write"
(check-equal? (first (get-token "write" token-matchers)) 'WRITE)
(check-equal? (first (get-token "write\n\n" token-matchers)) 'WRITE)
(check-equal? (first (get-token "writeabc" token-matchers)) 'WRITE)
(check-equal? (first (get-token "write123" token-matchers)) 'WRITE)

; get-token ... GOTO -> "goto"
(check-equal? (first (get-token "goto" token-matchers)) 'GOTO)
(check-equal? (first (get-token "goto\n\n" token-matchers)) 'GOTO)
(check-equal? (first (get-token "gotoabc" token-matchers)) 'GOTO)
(check-equal? (first (get-token "goto123" token-matchers)) 'GOTO)

; get-token ... GOSUB -> "gosub"
(check-equal? (first (get-token "gosub" token-matchers)) 'GOSUB)
(check-equal? (first (get-token "gosub\n\n" token-matchers)) 'GOSUB)
(check-equal? (first (get-token "gosubabc" token-matchers)) 'GOSUB)
(check-equal? (first (get-token "gosub123" token-matchers)) 'GOSUB)

; get-token ... ID -> [a-zA-Z]
(check-equal? (first (get-token "abc" token-matchers)) 'ID)
(check-equal? (first (get-token "abc\n\n" token-matchers)) 'ID)
(check-equal? (first (get-token "abcabc" token-matchers)) 'ID)
(check-equal? (first (get-token "abc123" token-matchers)) 'ID)

; get-token ... COLON -> ":"
(check-equal? (first (get-token ":" token-matchers)) 'COLON)
(check-equal? (first (get-token ":\n\n" token-matchers)) 'COLON)
(check-equal? (first (get-token ":abc" token-matchers)) 'COLON)
(check-equal? (first (get-token ":123" token-matchers)) 'COLON)

; get-token ... THEN -> "then"
(check-equal? (first (get-token "then" token-matchers)) 'THEN)
(check-equal? (first (get-token "then\n\n" token-matchers)) 'THEN)
(check-equal? (first (get-token "thenabc" token-matchers)) 'THEN)
(check-equal? (first (get-token "then123" token-matchers)) 'THEN)

; get-token ... ASSIGN-OP -> "="
(check-equal? (first (get-token "=" token-matchers)) 'ASSIGN-OP)
(check-equal? (first (get-token "=\n\n" token-matchers)) 'ASSIGN-OP)
(check-equal? (first (get-token "=abc" token-matchers)) 'ASSIGN-OP)
(check-equal? (first (get-token "=123" token-matchers)) 'ASSIGN-OP)

; get-token ... PLUS -> "+"
(check-equal? (first (get-token "+" token-matchers)) 'PLUS)
(check-equal? (first (get-token "+\n\n" token-matchers)) 'PLUS)
(check-equal? (first (get-token "+abc" token-matchers)) 'PLUS)
(check-equal? (first (get-token "+123" token-matchers)) 'PLUS)

; get-token ... MINUS -> "-"
(check-equal? (first (get-token "-" token-matchers)) 'MINUS)
(check-equal? (first (get-token "-\n\n" token-matchers)) 'MINUS)
(check-equal? (first (get-token "-abc" token-matchers)) 'MINUS)
(check-equal? (first (get-token "-123" token-matchers)) 'MINUS)

; get-token ... LPAREN -> "("
(check-equal? (first (get-token "(" token-matchers)) 'LPAREN)
(check-equal? (first (get-token "(\n\n" token-matchers)) 'LPAREN)
(check-equal? (first (get-token "(abc" token-matchers)) 'LPAREN)
(check-equal? (first (get-token "(123" token-matchers)) 'LPAREN)

; get-token ... RPAREN -> ")"
(check-equal? (first (get-token ")" token-matchers)) 'RPAREN)
(check-equal? (first (get-token ")\n\n" token-matchers)) 'RPAREN)
(check-equal? (first (get-token ")abc" token-matchers)) 'RPAREN)
(check-equal? (first (get-token ")123" token-matchers)) 'RPAREN)

; get-token ... RETURN -> "return"
(check-equal? (first (get-token "return" token-matchers)) 'RETURN)
(check-equal? (first (get-token "return\n\n" token-matchers)) 'RETURN)
(check-equal? (first (get-token "returnabc" token-matchers)) 'RETURN)
(check-equal? (first (get-token "return123" token-matchers)) 'RETURN)

; get-token ... WS -> whitespace
(check-equal? (first (get-token " " token-matchers)) 'WS)
(check-equal? (first (get-token " \n\n" token-matchers)) 'WS)
(check-equal? (first (get-token " abc" token-matchers)) 'WS)
(check-equal? (first (get-token " 123" token-matchers)) 'WS)