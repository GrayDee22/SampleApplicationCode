#include "eval.h"
#include "mathFunc.h"
#include "userFunc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Create a new symbol
sExp *createSymbol(const char *name) {
    sExp *sym = createEmptysExp(SYMBOL);
    sym->symbol = strdup(name);
    return sym;
}

// Evaluate each element in a list recursively
sExp *evalRest(sExp *rest, sExp **rho) {
    if (checkNil(rest)){
        return createEmptysExp(NIL);
    }

    if (rest->type != CONS){
        return copySExp(rest); //handles improper list
    }
    sExp *headEval = eval(car(rest), rho);
    sExp *tailEval = evalRest(cdr(rest), rho);
    sExp *result = cons(headEval, tailEval);
    return result;
}

// Evaluate an S-expression
//Returns a copy of the S-Expression so result can be freed in repl safely
// to prevent accidently freeing something in rho if the same sExp passed in gets returned
sExp *eval(sExp *sExpr, sExp **rho) {
    if (checkNil(sExpr)){
        return createEmptysExp(NIL);
    }

     // Numbers or strings evaluate to themselves
    if (checkNumber(sExpr) || checkString(sExpr)){
        return copySExp(sExpr);
    } 

    // Symbol lookup
    if (checkSymbol(sExpr)) {
        sExp *val = lookup(sExpr, *rho);
        if (!val) {
            return copySExp(sExpr);
        }
        return copySExp(val); //always return a copy
    }

    // List evaluation
    if (checkList(sExpr)) {
        sExp *function = car(sExpr); //function->symbol = the function name
        sExp *rest = cdr(sExpr);//Rest of the list

        if (!checkSymbol(function)){
            return copySExp(sExpr);
        }

        sExp *val = lookup(function, *rho); //Calls lookup to search the env

        if (val && val->type == USER_FUNC) { //If it is a user function,calls it
            return callFunc(val->ufunc, rest, rho, sExpr);
        }

        // Special forms that don't evaluate arguments first
        if (strcmp(function->symbol, "quote") == 0 ||
            strcmp(function->symbol, "set") == 0||
            strcmp(function->symbol, "if") == 0 ||
            strcmp(function->symbol, "cond") == 0 ||
            strcmp(function->symbol, "define") == 0) {
                return lispFunctions(sExpr, rho); //Don't copy here as these functions already copy
        }

        sExp *evaluatedRest = evalRest(rest, rho);
        sExp *newSexpr = cons(copySExp(function), evaluatedRest);

        sExp *result = copySExp(lispFunctions(newSexpr, rho));
        freeSExp(newSexpr);
        return copySExp(result); //Returns a copy always for repl to free properly

        //Error check
        printf("If reached, error in eval occured\n");
    }

    return copySExp(sExpr); //Returns the copy
}

//makes a 2-element list
sExp *list2(sExp *a, sExp *b) {
    return cons(a, cons(b, createEmptysExp(NIL)));
}

// Set a variable in the environment
sExp *set(sExp *symbol, sExp *value, sExp *rho) {
    sExp *envSymbols = car(rho);
    sExp *envValues  = cadr(rho);

    // Copy the symbol
    sExp *symbolCopy = createEmptysExp(SYMBOL);
    symbolCopy->symbol = strdup(symbol->symbol);

    // Copy the value into environment
    sExp *valueCopy = copySExp(value);

    envSymbols = cons(symbolCopy, envSymbols);
    envValues  = cons(valueCopy, envValues);

    return list2(envSymbols, envValues); //list2 creates the list of lists
}

// Lookup a symbol in the environment
sExp *lookup(sExp *symbol, sExp *rho) {
    if (checkNil(rho) || checkNil(car(rho))){
        return NULL;
    }

    sExp *envSymbols = car(rho);
    sExp *envValues  = cadr(rho);

    while (!checkNil(envSymbols)) {
        sExp *currSym = car(envSymbols);
        if (currSym && currSym->type == SYMBOL && strcmp(currSym->symbol, symbol->symbol) == 0) { //If the symbol name is in the env
            return copySExp(car(envValues)); // return the copy of the stored value
        }
        envSymbols = cdr(envSymbols);
        envValues = cdr(envValues);
    }
    return NULL;
}

// Deep copy of an S-expression
sExp *copySExp(sExp *sExpr) {
    if (!sExpr){
        return NULL;
    }

    sExp *copy = createEmptysExp(sExpr->type); //Creates a copy sExp that has the type of the passed in sExp
    switch (sExpr->type) {
        case LONG:
            copy->longInt = sExpr->longInt;
            break;
        case DOUBLE:
            copy->doubleInt = sExpr->doubleInt;
            break;
        case SYMBOL:
            copy->symbol = strdup(sExpr->symbol);
            break;
        case STRING:
            copy->string = strdup(sExpr->string);
            break;
        case CONS:
            copy->car = copySExp(car(sExpr));
            copy->cdr = copySExp(cdr(sExpr));
            break;
        case NIL://Doesn't need to return anything since the copy is already intalized to an empty sExp
        case USER_FUNC:
            copy->ufunc = sExpr->ufunc; //copy pointer to function def
            break;
        default:
            break;
    }
    return copy;
}

sExp *and(sExp *list, sExp **rho){
    sExp *a, *b;
    getSExpFromList(list, &a, &b);

    if (a == NULL || b == NULL){
        return createEmptysExp(NIL);//Returns nil
    }

    sExp *evaluatedA = eval(a, rho);
    if(checkNil(evaluatedA)){//If evaluation of a = NIL, then 'and' fails
        freeSExp(evaluatedA);
        return createEmptysExp(NIL);
    }

    freeSExp(evaluatedA);
    return eval(b, rho); //Returns whatever is evaluated in b since a != NIL
}

sExp *or(sExp *list, sExp **rho){
    sExp *a, *b;
    getSExpFromList(list, &a, &b);

    if (a == NULL || b == NULL){
        return createEmptysExp(NIL);//Returns nil
    }

    sExp *evaluatedA = eval(a, rho);
    if(!checkNil(evaluatedA)){//If a != NIL returns 't
        freeSExp(evaluatedA);
        return createSymbol("t");
    }

    freeSExp(evaluatedA);
    return eval(b, rho);
}

sExp *lispIf(sExp *list, sExp **rho){
    sExp *a, *b, *c;
    get3SExpFromList(list, &a, &b, &c);

    if (a == NULL){
        return createEmptysExp(NIL);//Returns nil
    }

    sExp *evaluatedA = eval(a, rho); //Evaluates arg1
    sExp *result = NULL;

    if(!checkNil(evaluatedA)){//If arg1 isn't nil returns the evaluated value of arg2
        if(b != NULL){
            result = eval(b, rho);//Eval if branch
        }
        else{
            result = createEmptysExp(NIL); //Missing if branch
        }
    }
    else{//Otherwise
        if(c != NULL){
            result = eval(c, rho); // Eval else branch
        }
        else{
            result = createEmptysExp(NIL); //Missing else branch
        }
    }
    freeSExp(evaluatedA);
    return result;
}

//Created since the pairs in cond don't have a function name
void getPairFromList(sExp *list, sExp **a, sExp **b) {
    *a = NULL;
    *b = NULL;

    if (checkNil(list)) {
        return; // Empty list, nothing to extract
    }

    *a = car(list);

    if (!checkNil(cdr(list))) {
        *b = cadr(list);
    }
}

sExp *cond(sExp *list, sExp **rho){
    //skips cond symbol
    if (!checkNil(list) && checkSymbol(car(list)) && strcmp(car(list)->symbol, "cond") == 0) {
        list = cdr(list);
    }
    while (!checkNil(list)) { // Iterate through each pair
        sExp *pair = car(list); //Pair that contains the test and result expressions

        if (!checkList(pair) || checkNil(pair)) {//Skips invalid pairs
            list = cdr(list);
            continue; // skip malformed clause
        }

        // Extract test and result expressions from the pair
        sExp *a, *b;
        getPairFromList(pair, &a, &b);

        if (a == NULL || b == NULL) {
            list = cdr(list);
            continue; // skip malformed pair
        }

        //If the test is true by default
        if (a->type == SYMBOL && strcmp(a->symbol, "'t") == 0) {
            return eval(b, rho);
        }

        sExp *evalTest = eval(a, rho); //Evals the test
        if (!checkNil(evalTest)) {//Same logic from the lispIf function
            freeSExp(evalTest);
            return eval(b, rho);
        }
        freeSExp(evalTest);

        // Move to the next pair
        list = cdr(list);
    }

    // If no pair passed, and the 't test wasn't included in the cond function
    //printf("\'t was not included inside the cond function");
    return createEmptysExp(NIL); //Maybe change to return list
}

sExp *define(sExp *fname, sExp *args, sExp *body, sExp **rho) {
    if (!checkSymbol(fname)) { //Checks if a function name was given if not returns nil
        printf("Error: define requires a symbol for function name\n");
        return createEmptysExp(NIL);
    }
    userFunc *f = makeFunc(fname, args, body); //Create the userFunc
    if (!f){
        return createEmptysExp(NIL);
    }

    //Convert it to an sExp of the type of a user function so the env can store it
    sExp *userFuncSExp = createEmptysExp(USER_FUNC);
    userFuncSExp->ufunc = f;

    *rho = set(fname, userFuncSExp, *rho);//Add to environment

    sExp *statement = createSymbol("Function Created"); 
    return statement; //Returns Function Created if the define worked
}

int countList(sExp *list) {
    int count = 0;
    while (!checkNil(list)) {
        count++;
        list = cdr(list);
    }
    return count;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Calls the Functions
//Only works for lowercased symbols
sExp *lispFunctions(sExp *list, sExp **rho){
    if(list == NULL || !checkSymbol(car(list))){
        return copySExp(list);
    }

    char *fname = car(list)->symbol; //sets the function name to the symbol for comparison
    sExp *rest = cdr(list); //What follows the function name
    sExp *arg = cadr(list); //Argument for the predicate function
    bool result = false;
        

    if (strcmp(fname, "quote") == 0) {
        return copySExp(car(rest));
    }
    else if (strcmp(fname, "set") == 0) {
        sExp *argSet = car(rest);
        sExp *valSExpr = cadr(rest);
        if (!argSet || !valSExpr) {
            printf("Set requires 2 arguments\n");
            return copySExp(list);
        }
        sExp *val = eval(valSExpr, rho);
        if (!val){
            return copySExp(list);
        }
        *rho = set(argSet, val, *rho); // store a copy safely
        return val;
    }

    //User Functions
    else if (strcmp(fname, "define") == 0) {
        if (checkNil(rest)) {
            printf("Error: define requires a name and a body\n");
            return createEmptysExp(NIL);
        }

        sExp *funcName = NULL;
        sExp *funcArgs = createEmptysExp(NIL);
        sExp *funcBody = createEmptysExp(NIL);

        sExp *first = car(rest);
        sExp *remaining = cdr(rest);

        if (first->type == CONS) {
            // Case 1: (define (foo x y) body)
            funcName = car(first);
            funcArgs = cdr(first);
            funcBody = remaining;
        } else if (checkSymbol(first)) {
            // Case 2: (define foo sExp)
            funcName = first;
            if (!checkNil(remaining)) {
                sExp *maybeArgs = car(remaining);
                if (maybeArgs->type == CONS) {
                    funcArgs = maybeArgs;
                    funcBody = cdr(remaining);
                } else {
                    // Simple variable binding
                    funcArgs = createEmptysExp(NIL);
                    funcBody = remaining;
                }
            }
        } else {
            printf("Error: define requires a symbol or a function list\n");
            return copySExp(list);
        }

        userFunc *f = makeFunc(funcName, funcArgs, funcBody); //Makes the function
        if (f == NULL) {
            return createEmptysExp(NIL);
        }

        sExp *funcSExp = createEmptysExp(USER_FUNC);  //creates an sExp for it
        funcSExp->ufunc = f;

        *rho = set(funcName, funcSExp, *rho); //adds it to the env

        return createSymbol("Function Created");
    }

    else if (strcmp(fname, "lambda") == 0) {
        sExp *args = car(rest);
        sExp *body = cdr(rest);

        userFunc *f = makeFunc(NULL, args, body); //calls a user function without a name which is a lambda
        if(!f){
            return createEmptysExp(NIL);
        }

        sExp *lambdaSExp = createEmptysExp(USER_FUNC);
        lambdaSExp->ufunc = f;

        return createSymbol("Lambda Created"); // return the function object
    }

    //Predicate Functions
    else if (strcmp(fname, "nil?") == 0) {
        result = checkNil(arg);
        return makeBoolResult(result);
    } else if (strcmp(fname, "symbol?") == 0) {
        result = checkSymbol(arg);
        return makeBoolResult(result);
    } else if (strcmp(fname, "number?") == 0) {
        result = checkNumber(arg);
        return makeBoolResult(result);
    } else if (strcmp(fname, "string?") == 0) {
        result = checkString(arg);
        return makeBoolResult(result);
    } else if (strcmp(fname, "list?") == 0) {
        result = checkList(arg);
        return makeBoolResult(result);
    } 
    
    //Short-Circuiting and Conditionals
    //No need to make a copy as these all eval inside the functions, and eval already returns a copy
    else if (strcmp(fname, "and") == 0) {
        return and(list, rho);
    } 
    else if (strcmp(fname, "or") == 0) {
        return or(list, rho);
    }
    else if (strcmp(fname, "if") == 0) {
        return lispIf(list, rho);
    }
    else if (strcmp(fname, "cond") == 0) {
        return cond(list, rho);
    }

    //Math Functions
    else if (strcmp(fname, "add") == 0 || strcmp(fname, "+") == 0) {
        return copySExp(add(list, rho));
    } 
    else if (strcmp(fname, "sub") == 0 || strcmp(fname, "-") == 0) {
        return copySExp(sub(list, rho));
    } 
    else if (strcmp(fname, "mul") == 0 || strcmp(fname, "*") == 0) {
        return copySExp(mul(list, rho));
    } 
    else if (strcmp(fname, "div") == 0 || strcmp(fname, "/") == 0) {
        return copySExp(divide(list, rho));
    } 
    else if (strcmp(fname, "mod") == 0) {
        return copySExp(mod(list, rho));
    } 
    else if (strcmp(fname, "lt") == 0) {
        return copySExp(lt(list, rho));
    } 
    else if (strcmp(fname, "gt") == 0) {
        return copySExp(gt(list, rho));
    } 
    else if (strcmp(fname, "lte") == 0) {
        return copySExp(lte(list, rho));
    } 
    else if (strcmp(fname, "gte") == 0) {
        return copySExp(gte(list, rho));
    } 
    else if (strcmp(fname, "eq") == 0) {
        return copySExp(eq(list, rho));
    }
    else if (strcmp(fname, "not") == 0) {
        return copySExp(logicalNot(list, rho));
    }

    //Lisp Functions
    else if (strcmp(fname, "cons") == 0) {
        sExp *arg2 = NULL;
        if (cdr(list) != NULL && cdr(cdr(list)) != NULL) {
            arg2 = caddr(list);
        }
        return copySExp(cons(arg, arg2));
    } 
    else if (strcmp(fname, "car") == 0) {
        if (arg == NULL || arg->type != CONS) {
            return createEmptysExp(NIL); // return nil
        }
        return copySExp(car(arg));
    }
    else if (strcmp(fname, "cdr") == 0) {
        if (arg == NULL || arg->type != CONS) {
            return createEmptysExp(NIL); // return nil
        }
        return copySExp(cdr(arg));
    }
    else if (strcmp(fname, "cadr") == 0) {
        if (arg == NULL || arg->type != CONS) {
            return createEmptysExp(NIL); // return nil
        }
        return copySExp(cadr(arg));
    }
    else if (strcmp(fname, "cdar") == 0) {
        if (arg == NULL || arg->type != CONS) {
            return createEmptysExp(NIL); // return nil
        }
        return copySExp(cdar(arg));
    }
    else if (strcmp(fname, "caddr") == 0) {
        if (arg == NULL || arg->type != CONS) {
            return createEmptysExp(NIL); // return nil
        }
        return copySExp(caddr(arg));
    }


    // User-defined functions & lambda
    sExp *userFuncVal = lookup(car(list), *rho);
    if (userFuncVal && userFuncVal->type == USER_FUNC) {
        userFunc *f = userFuncVal->ufunc;
        // Check argument count
        int expected = countList(f->args);
        int provided = countList(rest);
        if (expected != provided) {
            if (fname != NULL) {
                printf("Warning: argument count mismatch in function %s\n", fname);
            } else {
                printf("Warning: argument count mismatch in anonymous function\n");
            }
        }
        return callFunc(f, rest, rho, list);
    }

    //Exit
    else if (strcmp(fname, "exit") == 0) {
        printf("Lisp interpreter is now over\n");
        exit(0);
    }
    
    return copySExp(list); //Returns the orginal S-Expression if it can't pass any of the function calls    
}