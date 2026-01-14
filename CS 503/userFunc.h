#ifndef USERFUNC_H
#define USERFUNC_H

#include <stdio.h>
#include <stdbool.h>

#include "parser.h"

// Structure for User Defined Functions
typedef struct userFunc {
    char *fname;
    sExp *args;
    sExp *body;

} userFunc;

userFunc *makeFunc(sExp *fname, sExp *args, sExp *body);
sExp *reverseSExp(sExp *list);
sExp *evalSequence(sExp *exprs, sExp **rho);
sExp *callFunc(userFunc *f, sExp *argsList, sExp **rho, sExp *original);

#endif