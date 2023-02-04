
#lang racket

(require rackunit "scan.rkt")

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

; idx?
(check-equal? (idx? "123") (list 'IDX "123"))
(check-equal? (idx? "123 ") (list 'IDX "123"))
(check-equal? (idx? "123 123") (list 'IDX "123"))
(check-equal? (idx? "123a123") (list 'IDX "123"))
(check-equal? (idx? " 123") #f)
(check-equal? (idx? "0123") #f)
(check-equal? (idx? "-123") #f)

; colon?
(check-equal? (colon? ":") (list 'COLON ":"))
(check-equal? (colon? ": ") (list 'COLON ":"))
(check-equal? (colon? "::") (list 'COLON ":"))
(check-equal? (colon? " :") #f)

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