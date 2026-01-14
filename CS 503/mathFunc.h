#ifndef MATHFUNC_H
#define MATHFUNC_H

#include "parser.h"
#include "eval.h"
#include <stdio.h>

//Helper Functiosn
// Gets the two S-expressions from a list
void getSExpFromList(sExp *list, sExp **a, sExp **b);
//Gets 3 S-expressions from a list
void get3SExpFromList(sExp *list, sExp **a, sExp **b, sExp **c);

void evalTwoSExps(sExp **a, sExp **b, sExp **rho);
void evalThreeSExps(sExp **a, sExp **b, sExp **c, sExp **rho);

double convertToDouble(sExp* num);
sExp* typeCastResult(double val);
sExp* makeBoolResult(int check);
bool checkBothMathArgs(sExp *a, sExp *b, const char *fname);

sExp *add(sExp *list, sExp **rho);
sExp *sub(sExp *list, sExp **rho);
sExp *mul(sExp *list, sExp **rho);
sExp *divide(sExp *list, sExp **rho);
sExp *mod(sExp *list, sExp **rho);

sExp *lt(sExp *list, sExp **rho);
sExp *gt(sExp *list, sExp **rho);
sExp *lte(sExp *list, sExp **rho);
sExp *gte(sExp *list, sExp **rho);

bool eqTwoArgs(sExp *a, sExp *b);
sExp *eq(sExp *list, sExp **rho);
sExp *logicalNot(sExp *list, sExp **rho);

#endif
