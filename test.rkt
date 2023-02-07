
#lang racket

(require rackunit "scan.rkt")

; ws?
(check-equal? (ws? " \t") (list 'WS " \t"))
(check-equal? (ws? " \ta") (list 'WS " \t"))
(check-equal? (ws? "a \t") #f)

; eop?
(check-equal? (eop? "$$") (list 'EOP "$$"))
(check-equal? (eop? "$$ ") (list 'EOP "$$"))
(check-equal? (eop? "$$$$") (list 'EOP "$$"))
(check-equal? (eop? " $$") #f)

; eol?
(check-equal? (eol? "\n") (list 'EOL "\n"))
(check-equal? (eol? "\n ") (list 'EOL "\n"))
(check-equal? (eol? "\n\n") (list 'EOL "\n"))
(check-equal? (eol? " \n") #f)

; colon?
(check-equal? (colon? ":") (list 'COLON ":"))
(check-equal? (colon? ": ") (list 'COLON ":"))
(check-equal? (colon? "::") (list 'COLON ":"))
(check-equal? (colon? " :") #f)

; id?
(check-equal? (id? "abcABC") (list 'ID "abcABC"))
(check-equal? (id? "abcABC ") (list 'ID "abcABC"))
(check-equal? (id? "abcABC abcABC") (list 'ID "abcABC"))
(check-equal? (id? "abcABC123") (list 'ID "abcABC"))
(check-equal? (id? " abcABC") #f)
(check-equal? (id? "0abcABC") #f)
(check-equal? (id? " abcABC") #f)

; assign-op?
(check-equal? (assign-op? "=") (list 'ASSIGN-OP "="))
(check-equal? (assign-op? "= ") (list 'ASSIGN-OP "="))
(check-equal? (assign-op? "==") (list 'ASSIGN-OP "="))
(check-equal? (assign-op? " =") #f)

; if?
(check-equal? (if? "if") (list 'IF "if"))
(check-equal? (if? "if ") (list 'IF "if"))
(check-equal? (if? "ifif") (list 'IF "if"))
(check-equal? (if? " if") #f)

; then?
(check-equal? (then? "then") (list 'THEN "then"))
(check-equal? (then? "then ") (list 'THEN "then"))
(check-equal? (then? "thenthen") (list 'THEN "then"))
(check-equal? (then? " then") #f)

; read?
(check-equal? (read? "read") (list 'READ "read"))
(check-equal? (read? "read ") (list 'READ "read"))
(check-equal? (read? "readread") (list 'READ "read"))
(check-equal? (read? " read") #f)

; write?
(check-equal? (write? "write") (list 'WRITE "write"))
(check-equal? (write? "write ") (list 'WRITE "write"))
(check-equal? (write? "writewrite") (list 'WRITE "write"))
(check-equal? (write? " write") #f)

; goto?
(check-equal? (goto? "goto") (list 'GOTO "goto"))
(check-equal? (goto? "goto ") (list 'GOTO "goto"))
(check-equal? (goto? "gotogoto") (list 'GOTO "goto"))
(check-equal? (goto? " goto") #f)

; gosub?
(check-equal? (gosub? "gosub") (list 'GOSUB "gosub"))
(check-equal? (gosub? "gosub ") (list 'GOSUB "gosub"))
(check-equal? (gosub? "gosubgosub") (list 'GOSUB "gosub"))
(check-equal? (gosub? " gosub") #f)

; return?
(check-equal? (return? "return") (list 'RETURN "return"))
(check-equal? (return? "return ") (list 'RETURN "return"))
(check-equal? (return? "returnreturn") (list 'RETURN "return"))
(check-equal? (return? " return") #f)

; lparen?
(check-equal? (lparen? "(") (list 'LPAREN "("))
(check-equal? (lparen? "( ") (list 'LPAREN "("))
(check-equal? (lparen? "((") (list 'LPAREN "("))
(check-equal? (lparen? " (") #f)

; rparen?
(check-equal? (rparen? ")") (list 'RPAREN ")"))
(check-equal? (rparen? ") ") (list 'RPAREN ")"))
(check-equal? (rparen? "))") (list 'RPAREN ")"))
(check-equal? (rparen? " )") #f)

; zero-digit?
(check-equal? (zero-digit? "0") (list 'ZERO-DIGIT "0"))
(check-equal? (zero-digit? "0 ") (list 'ZERO-DIGIT "0"))
(check-equal? (zero-digit? "00") (list 'ZERO-DIGIT "0"))
(check-equal? (zero-digit? " 0") #f)

; nonzero-digit?
(check-equal? (nonzero-digit? "0") #f)

(check-equal? (nonzero-digit? "1") (list 'NONZERO-DIGIT "1"))
(check-equal? (nonzero-digit? "1 ") (list 'NONZERO-DIGIT "1"))
(check-equal? (nonzero-digit? "11") (list 'NONZERO-DIGIT "1"))
(check-equal? (nonzero-digit? " 1") #f)

(check-equal? (nonzero-digit? "2") (list 'NONZERO-DIGIT "2"))
(check-equal? (nonzero-digit? "2 ") (list 'NONZERO-DIGIT "2"))
(check-equal? (nonzero-digit? "22") (list 'NONZERO-DIGIT "2"))
(check-equal? (nonzero-digit? " 2") #f)

(check-equal? (nonzero-digit? "3") (list 'NONZERO-DIGIT "3"))
(check-equal? (nonzero-digit? "3 ") (list 'NONZERO-DIGIT "3"))
(check-equal? (nonzero-digit? "33") (list 'NONZERO-DIGIT "3"))
(check-equal? (nonzero-digit? " 3") #f)

(check-equal? (nonzero-digit? "4") (list 'NONZERO-DIGIT "4"))
(check-equal? (nonzero-digit? "4 ") (list 'NONZERO-DIGIT "4"))
(check-equal? (nonzero-digit? "44") (list 'NONZERO-DIGIT "4"))
(check-equal? (nonzero-digit? " 4") #f)

(check-equal? (nonzero-digit? "5") (list 'NONZERO-DIGIT "5"))
(check-equal? (nonzero-digit? "5 ") (list 'NONZERO-DIGIT "5"))
(check-equal? (nonzero-digit? "55") (list 'NONZERO-DIGIT "5"))
(check-equal? (nonzero-digit? " 5") #f)

(check-equal? (nonzero-digit? "6") (list 'NONZERO-DIGIT "6"))
(check-equal? (nonzero-digit? "6 ") (list 'NONZERO-DIGIT "6"))
(check-equal? (nonzero-digit? "66") (list 'NONZERO-DIGIT "6"))
(check-equal? (nonzero-digit? " 6") #f)

(check-equal? (nonzero-digit? "7") (list 'NONZERO-DIGIT "7"))
(check-equal? (nonzero-digit? "7 ") (list 'NONZERO-DIGIT "7"))
(check-equal? (nonzero-digit? "77") (list 'NONZERO-DIGIT "7"))
(check-equal? (nonzero-digit? " 7") #f)