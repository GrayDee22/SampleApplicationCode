#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>

#include "parser.h"
#include "mathFunc.h"
#include "eval.h"
#include "userFunc.h"

// Main function
int main(int argc, char ** argv) {
    FILE* input;

    if (argc == 2) {
        input = fopen(argv[1], "r");
        if (input == NULL) {
            printf("Invalid input file\n");
            return 1;
        }
    } else if(argc == 1){
        input = stdin;
    }  else {
        printf("Invalild amount of arguments\n");
        return 1;
    }

    // Initialize environment
    sExp *symbols = createEmptysExp(NIL);
    sExp *values  = createEmptysExp(NIL);

    // For t and nil
    symbols = cons(createSymbol("t"), symbols);
    values  = cons(createSymbol("t"), values); //t evaluates to itself with signals true
    symbols = cons(createSymbol("nil"), symbols);
    values  = cons(createEmptysExp(NIL), values);

    sExp *rho = list2(symbols, values);
    printf("Parsing input:\n\n");

    //starts REPL
    repl(input, &rho); //pass the env and input
    freeSExp(rho); //Frees the env after repl is done
   
    if (input != stdin) {
        fclose(input); //only close if input is a file
    }
    return 0;
}
