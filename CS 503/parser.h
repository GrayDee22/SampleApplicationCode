#ifndef PARSER_H
#define PARSER_H

#include <stdio.h>
#include <stdbool.h>

// Types of S-expressions
enum sExpType {
    LONG,   //1
    DOUBLE, //2
    SYMBOL, //3
    STRING, //4
    CONS,    //5
    NIL, //6
    USER_FUNC //7
};

// Structure for S-expressions
typedef struct sExp {
    enum sExpType type; //1-5
    long longInt;
    double doubleInt;
    char *symbol;
    char *string;
    struct sExp *car;
    struct sExp *cdr;
    struct userFunc *ufunc; //for USER_FUNC
} sExp; 


// Function declarations
sExp *atomSorter(char **line);
sExp *sExpSorter(char **line);
sExp *parseList(char **line);
void printSExp(sExp *sExpr);
void freeSExp(sExp *sExpr);

// Accessor and cons helpers
sExp *createEmptysExp(enum sExpType type);
sExp *cons(sExp* carVal, sExp* cdrVal);
sExp *car(sExp* list);
sExp *cdr(sExp* list);
sExp *cadr(sExp* list);
sExp *cdar(sExp* list);
sExp *caddr(sExp* list);
sExp *cadddr(sExp* list);

// Predicates
bool checkNil(sExp *sExpr);
bool checkSymbol(sExp *sExpr);
bool checkNumber(sExp *sExpr);
bool checkString(sExp *sExpr);
bool checkList(sExp *sExpr);

int parenthesesCounter(const char *sExp);
char *stripComments(char *line);
void repl(FILE *in, sExp **rho);

#endif