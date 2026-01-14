# Lisp-Parser In C (Sprints 1-8)
* Overview:
- This project implements a basic Lisp parser in C.
  It supports:
    - Parsing S-expressions (atoms and lists)
    - Predicates to identify types (nil?, number?, symbol?, string?, list?)
    - Basic math and logical operations (+, -, *, /, %, >, <, <=, >=, eq, not)
    - The eval function
    - Short-Circuiting Functions & Conditional Statements (and, or, if, cond)
    - User Functions

- The code is structured into modules for parsing, predicates, and math functions. 
- Memory management and printing of S-expressions are also handled.

* File Structure
- lispMain.c      : Main
- parser.c        : S-expression parser implementation, along with predicate implementation
- parser.h        : Header for parser.c
- mathFunc.c      : Arithmetic and logical function implementations
- mathFunc.h      : Header for mathFunc.c
- eval.h          : Header for eval.
- eval.c          : eval, and, or, if, cond, define, and lispFunctions implementations
- userFunc.h      : Header for userFunc.c
- userFunc.c      : user functions implementation
- input.txt       : Sample input file for testing all sprints
- output.txt      : Generated output file
- README.txt      : This documentation

# Compliation
- Compile with:
   - gcc lispMain.c parser.c mathFunc.c eval.c userFunc.c
   OR with
   - gcc lispMain.c parser.c mathFunc.c eval.c userFunc.c -o lisp (If you want an output file)
- Run with:
   - ./a.out input.txt
   OR with
   - or just run with ./a.out if you want to run without a file
   OR with 
   - ./lisp input.txt > output.txt (If you want an output file)
         -  Output is written to output.txt.