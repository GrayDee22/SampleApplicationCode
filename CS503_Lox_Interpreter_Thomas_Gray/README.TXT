# Lox-Interpreter in clojure 
*Overview:
- This project is a implementation of a Lox Interpreter written in Clojure, based upon the book Crafting Interpreters (Chapters 4-13) by Robert Nystrom.
    It support: 
        - Scanning, parsing, and evaluating input
        - Major Lox features, such as:
            -  Expressions
            -  Control flow
            -  Functions (Built in and user-defined)
            -  Classes, methods, this, super
            -  Inheritance
        - A clock (A Lox built-in native function)
        - Descriptive error handling for syntax and runtime errors
        - A built in Lox REPL & the ability to read in from a file as well
        - Recursive descent parsing
        - Proper environment and scope handling (including closures)

* File Structure: (Note: Files are organized in Lox_Interpreter/src/lox/...)
- loxMain.clj        Creates the REPL, or reads input from a file. Also creates a globalEnv to start the interpreter with, and also counts {}'s in the inupt (Can only continue if the opens and closes counts match)
- scanner.clj        Converts raw input into tokens
- parser.clj         Recursive descent parser that builds an abstract syntax tree (AST)
- error.clj          Error handling for syntax
- runtimeError.clj   Runtime error handling
- expr.clj           Expression definitions and visitor interface
- stmt.clj           Statement definitions and visitor interface
- environment.clj    Creates an environment, clock, and handles variable scoping and lookups
- interpreter.clj    Evaluates expressions and executes statements
- resolver.clj.      Checks where variables are defined to catch scope errors
- LoxCallable.clj    Represents anything in Lox that can be called like a function or class
- LoxFunction.clj    Function object and call logic, including closures
- LoxClass.clj.      Class object and method binding logic
- LoxInstance.clj    Instance field storage and method access
- input.txt          Input file with Lox code for testing

# Compliation
- Compile with: 
        clojure -M -m lox.loxMain (generates a REPL to test code in)
- OR with:
        clojure -M -m lox.loxMain [filename.txt] (reads in a file)