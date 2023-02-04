
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