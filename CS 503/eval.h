#ifndef EVAL_H
#define EVAL_H

#include <stdio.h>
#include "parser.h"

sExp *createSymbol(const char *name);
sExp *eval(sExp *sExpr, sExp **rho);
sExp *evalRest(sExp *rest, sExp **rho);
sExp *list2(sExp *a, sExp *b);
sExp *set(sExp *symbol, sExp *value, sExp *rho);
sExp *lookup(sExp *symbol, sExp *rho);
sExp *copySExp(sExp *sExpr);
int countList(sExp *list);
sExp *lispFunctions(sExp *list, sExp **rho);

//Short-Circuiting and Conditionals
sExp *and(sExp *list, sExp **rho);
sExp *or(sExp *list, sExp **rho);
sExp *lispIf(sExp *list, sExp **rho);
void getPairFromList(sExp *list, sExp **a, sExp **b);
sExp *cond(sExp *list, sExp **rho);

//Functions
sExp *define(sExp *fname, sExp *args, sExp *body, sExp **rho);

#endif