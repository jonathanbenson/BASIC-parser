# BASIC Parser

### Summary
A recursive-descent parser for a simplified version of the early BASIC programming language.

### Grammar
`
program -> linelist $$ 

linelist -> line linelist | epsilon 

line -> idx stmt linetail* [EOL]

idx -> nonzero_digit digit* 

linetail -> :stmt | epsilon 

stmt -> id = expr | if expr then stmt | read id | write expr | goto idx | gosub idx | return

expr -> id etail | num etail | (expr)

etail -> + expr | - expr | = expr | epsilon

id -> [a-zA-Z]+

num -> numsign digit digit*

numsign -> + | - | epsilon 

nonzero_digit -> 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

digit -> 0 | nonzero_digit
`

### Dependencies
- Racket v8.5

### Execution
1. Navigate to the project directory.
2. Run `racket test.rkt` to test the project.
3. Run `racket main.rkt` to run the project.
    - Modify the function call in main.rkt `parse <file-path>` to parse a specific file.


