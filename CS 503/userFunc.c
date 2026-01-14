#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "parser.h"
#include "mathFunc.h"
#include "eval.h"
#include "userFunc.h"


userFunc *makeFunc(sExp *fname, sExp *args, sExp *body) {
    userFunc *f = malloc(sizeof(userFunc));
    if (!f) {
        printf("Error: could not allocate userFunc\n");
        return NULL;
    }

    // Function name (NULL for lambda)
    if (fname && checkSymbol(fname)) {
        f->fname = strdup(fname->symbol);
    } else {
        f->fname = NULL;
    }

    // Ensure args is always a proper list of symbols
    if (!args || checkNil(args)) {
        f->args = createEmptysExp(NIL);
    } else if (args->type == SYMBOL) {
        f->args = cons(copySExp(args), createEmptysExp(NIL));
    } else if (args->type == CONS) {
        // Verify each element is a symbol
        sExp *curr = args;
        while (!checkNil(curr)) {
            if (!checkSymbol(car(curr))) {
                printf("Error: lambda argument is not a symbol\n");
                free(f);
                return NULL;
            }
            curr = cdr(curr);
        }
        f->args = copySExp(args);
    } else { //Invalid arguments
        printf("Error: invalid lambda argument list\n");
        free(f);
        return NULL;
    }

   if (body && !checkNil(body)) {
        f->body = copySExp(body);
    } else {
        f->body = createEmptysExp(NIL);
    }

    return f;
}

//creates a new list in reverse order
sExp *reverseSExp(sExp *list) {
    sExp *rev = createEmptysExp(NIL);
    if (checkNil(list)) {
        return rev;
    }
    while (!checkNil(list)) {
        rev = cons(copySExp(car(list)), rev);
        list = cdr(list);
    }
    return rev;
}

// Evaluate a sequence of expressions & returns the value of the last expression
sExp *evalSequence(sExp *exprs, sExp **rho) {
    if (checkNil(exprs)){
        return createEmptysExp(NIL);
    }

    sExp *result = NULL;
    while (!checkNil(exprs)) {
        if (exprs->type == CONS) {
            sExp *current = car(exprs);
            if (result){
                freeSExp(result);
            }
            result = eval(current, rho);
            exprs = cdr(exprs);
        } else {
            // Single expressions (non list)
            if (result) freeSExp(result);
            result = eval(exprs, rho);
            break;
        }
    }
    return result;
}

//Calls the function
sExp *callFunc(userFunc *f, sExp *argsList, sExp **rho, sExp *originalCall) {
    if (!f){
        return createEmptysExp(NIL);
    }

    sExp *evaluatedArgs = evalRest(argsList, rho);
    sExp *parameters = f->args;
    sExp *newRho = *rho;

    // Bind parameters to arguments
    while (!checkNil(parameters) && !checkNil(evaluatedArgs)) {
        sExp *parameterSym = car(parameters);
        sExp *argVal   = car(evaluatedArgs);

        if (!checkSymbol(parameterSym)) {
            printf("Error: function parameter is not a symbol\n");
            freeSExp(evaluatedArgs);
            return createEmptysExp(NIL);
        }

        newRho = set(parameterSym, copySExp(argVal), newRho);

        parameters = cdr(parameters);
        evaluatedArgs = cdr(evaluatedArgs);
    }

    // Check argument count mismatch
    if (!checkNil(parameters) || !checkNil(evaluatedArgs)) {
        if (f->fname) {
            printf("Warning: argument count mismatch in function %s\n", f->fname);
        } else {
            printf("Warning: argument count mismatch in anonymous function\n");
        }
        freeSExp(evaluatedArgs);
        return createEmptysExp(NIL);
    }

    sExp *result = evalSequence(f->body, &newRho);
    freeSExp(evaluatedArgs);
    return result;
}
