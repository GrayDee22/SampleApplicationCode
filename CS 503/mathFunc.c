#include "mathFunc.h"
#include "parser.h"
#include "eval.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Gets the two S-expressions from a list
//NOTE: Only works if there are two S-exp in the list after the symbol ie: (add 1 2) and not (add 1 2 3) as 3 will be ignored
void getSExpFromList(sExp *list, sExp **a, sExp **b) {
    *a = NULL;
    *b = NULL;

    if (checkNil(list)) {
        return; // Empty list, nothing to extract
    }

    if (!checkNil(cdr(list))) {
        *a = cadr(list);
    } 
    if (!checkNil(cdr(cdr(list)))) {
        *b = caddr(list);
    }
}

//Gets 3 S-exp from the list
void get3SExpFromList(sExp *list, sExp **a, sExp **b, sExp **c) {
    *a = NULL;
    *b = NULL;
    *c = NULL;

    if (checkNil(list)) {
        return; // Empty list, nothing to extract
    }
    
    if (!checkNil(cdr(list))) {
        *a = cadr(list);
    } 
    if (!checkNil(cdr(cdr(list)))) {
        *b = caddr(list);
    }
    if(!checkNil(cdr(cdr(cdr(list))))){
        *c = cadddr(list);
    }
}

// Helper: evaluate two S-expressions if not NULL
void evalTwoSExps(sExp **a, sExp **b, sExp **rho) {
    if (*a != NULL) {
        *a = eval(*a, rho);
    }
    if (*b != NULL) {
        *b = eval(*b, rho);
    }
}

// Helper: evaluate three S-expressions if not NULL
void evalThreeSExps(sExp **a, sExp **b, sExp **c, sExp **rho) {
    if (*a != NULL) {
        *a = eval(*a, rho);
    }
    if (*b != NULL) {
        *b = eval(*b, rho);
    }
    if (*c != NULL) {
        *c = eval(*c, rho);
    }
}

// Math functions
//Only works for two numbers, will return an error message, and the full list if the arguments aren't both numbers

// Convert both operands to double for calculation, regardless of their original type.
// After computing, determine if the result is integral (LONG) or has fraction (DOUBLE),
// and store the result accordingly.

//Helper Functions:
double convertToDouble(sExp* num){
    if (num->type == LONG) {
        return (double)num->longInt;
    } else { 
        return num->doubleInt;
    }
}
//Makes the result an S-Expression, and type casts it based on what the result is
sExp* typeCastResult(double val) {
    sExp* res = NULL;
    if ((val - (long)val) == 0.0) {
        res = createEmptysExp(LONG);
        res->longInt = (long)val;
    } else {
        res = createEmptysExp(DOUBLE);
        res->doubleInt = val;
    }
    return res;
}
//Helps return an S-expression boolean value
sExp* makeBoolResult(int check) {
    if (check) {
        return createSymbol("t");
    } else {
        return createEmptysExp(NIL);
    }
}

// Validates that both args are numbers.
// If not, prints an error and returns 0. Caller decides what to do on failure.
bool checkBothMathArgs(sExp *a, sExp *b, const char *fname) {
    if (!checkNumber(a) || !checkNumber(b)) {
        printf("%s: has invalid arguments\n", fname);
        return false; // failed
    }
    return true; // valid
}

sExp *add(sExp *list, sExp **rho) {
    sExp *a, *b;
    getSExpFromList(list, &a, &b);

    evalTwoSExps(&a, &b, rho);

    if(!checkBothMathArgs(a,b, "add")){
        return list;//Couldn't evaluate so returns the sExp
    }
    double sum = convertToDouble(a) + convertToDouble(b);
    freeSExp(a);
    freeSExp(b);
    return typeCastResult(sum);
}

sExp *sub(sExp *list, sExp **rho) {
    sExp *a, *b;
    getSExpFromList(list, &a, &b);

    evalTwoSExps(&a, &b, rho);

    if(!checkBothMathArgs(a,b, "sub")){
        return list;//Couldn't evaluate so returns the sExp
    }

    double difference = convertToDouble(a) - convertToDouble(b);
    freeSExp(a);
    freeSExp(b);
    return typeCastResult(difference);
}

sExp *mul(sExp *list, sExp **rho) {
    sExp *a, *b;
    getSExpFromList(list, &a, &b);

   evalTwoSExps(&a, &b, rho);

    if(!checkBothMathArgs(a,b, "mul")){
        return list;//Couldn't evaluate so returns the sExp
    }

    double product = convertToDouble(a) * convertToDouble(b);
    freeSExp(a);
    freeSExp(b);
    return typeCastResult(product);
}

sExp *divide(sExp *list, sExp **rho) {
    sExp *a, *b;
    getSExpFromList(list, &a, &b);

    evalTwoSExps(&a, &b, rho);

    if(!checkBothMathArgs(a,b, "div")){
        return list;//Couldn't evaluate so returns the sExp
    }
    double val2 = convertToDouble(b);
    if (val2 == 0.0) {
        printf("div: division by zero\n");
        freeSExp(a);
        freeSExp(b);
        return list;//Divided by 0, returns the sExp
    }

    double quotion = convertToDouble(a) / val2;
    freeSExp(a);
    freeSExp(b);
    return typeCastResult(quotion);
}

sExp *mod(sExp *list, sExp **rho) {
    sExp *a, *b;
    getSExpFromList(list, &a, &b);

    evalTwoSExps(&a, &b, rho);

    if (!a || !b || a->type != LONG || b->type != LONG) {
        printf("mod: invalid arguments (integers only)\n");
        freeSExp(a);
        freeSExp(b);
        return list;
    }

    if (b->longInt == 0) {
        printf("mod: division by zero\n");
        freeSExp(a);
        freeSExp(b);
        return list;
    }

    sExp *res = createEmptysExp(LONG);
    res->longInt = a->longInt % b->longInt;
    freeSExp(a);
    freeSExp(b);
    return res;
}

// Relational: lt, gt, lte, gte
sExp *lt(sExp *list, sExp **rho) {
    sExp *a;
    sExp *b;
    getSExpFromList(list, &a, &b);

    evalTwoSExps(&a, &b, rho);

    if (!checkBothMathArgs(a, b, "lt")){
        return list;//Couldn't evaluate so returns the sExp
    }

    int check = 0;
    if (convertToDouble(a) < convertToDouble(b)) {
        check = 1;
    }
    freeSExp(a);
    freeSExp(b);
    return makeBoolResult(check);
}

sExp *gt(sExp *list, sExp **rho) {
    sExp *a;
    sExp *b;
    getSExpFromList(list, &a, &b);

    evalTwoSExps(&a, &b, rho);

    if (!checkBothMathArgs(a, b, "gt")){
        return list;//Couldn't evaluate so returns the sExp
    }

    int check = 0;
    if (convertToDouble(a) > convertToDouble(b)) {
        check = 1;
    }
    freeSExp(a);
    freeSExp(b);
    return makeBoolResult(check);
}

sExp *lte(sExp *list, sExp **rho) {
    sExp *a;
    sExp *b;
    getSExpFromList(list, &a, &b);

    evalTwoSExps(&a, &b, rho);

    if (!checkBothMathArgs(a, b, "lte")){
        return list;//Couldn't evaluate so returns the sExp
    }

    int check = 0;
    if (convertToDouble(a) <= convertToDouble(b)) {
        check = 1;
    }
    freeSExp(a);
    freeSExp(b);
    return makeBoolResult(check);
}

sExp *gte(sExp *list, sExp **rho) {
    sExp *a;
    sExp *b;
    getSExpFromList(list, &a, &b);

    evalTwoSExps(&a, &b, rho);

    if (!checkBothMathArgs(a, b, "gte")){
        return list;//Couldn't evaluate so returns the sExp
    }

    int check = 0;
    if (convertToDouble(a) >= convertToDouble(b)) {
        check = 1;
    }
    freeSExp(a);
    freeSExp(b);
    return makeBoolResult(check);
}

// Checks if numbers, strings, or symbols are equivalent to one another
bool eqTwoArgs(sExp *a, sExp *b) {
    if (!a || !b){
        return false;
    }
    if (a->type != b->type){
        return false;
    }

    switch (a->type) {
        case SYMBOL:
            return strcmp(a->symbol, b->symbol) == 0;
        case LONG:
            return a->longInt == b->longInt;
        case DOUBLE:
            return a->doubleInt == b->doubleInt;
        case STRING:
            return strcmp(a->string, b->string) == 0;
        case NIL:
            return true; // all NILs are equal
        default:
            return a == b; // default for CONS
    }
}

sExp *eq(sExp *list, sExp **rho) {
    sExp *a = NULL;
    sExp *b = NULL;

    getSExpFromList(list, &a, &b);

    if (a == NULL || b == NULL) {
        printf("eq: invalid arguments\n");
        return NULL;
    }
    evalTwoSExps(&a, &b, rho);

    int check = eqTwoArgs(a, b);
    freeSExp(a);
    freeSExp(b);
    return makeBoolResult(check);
}

sExp *logicalNot(sExp *list, sExp **rho) {
    sExp *a = NULL, *b = NULL;
    getSExpFromList(list, &a, &b); //Should only return a, b is just there to pass in

    if (!a) {
        printf("not: missing argument\n");
        return createEmptysExp(NIL);
    }

    // Evaluate the argument
    sExp *evalA = eval(a, rho);
    int check = checkNil(evalA); // true if evalA is nil, false otherwise
    freeSExp(evalA);

    return makeBoolResult(check); // returns 't or nil
}


