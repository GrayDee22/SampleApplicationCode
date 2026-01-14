#include "parser.h"
#include "mathFunc.h" //for mathFunctions
#include "eval.h" //for eval

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// Predicate functions
//Checks if the S-exp is first valid, and then if it is nil, a symbol, number,...,etc based off of the enum type it is
bool checkNil(sExp *sExpr) {
    if (sExpr == NULL) {
        return true;
    }
    if (sExpr->type == NIL) {
        return true;
    }
    return false;
}

bool checkSymbol(sExp *sExpr) {
    if (sExpr != NULL) {
        if (sExpr->type == SYMBOL) {
            return true;
        } else {
            return false;
        }
    } else {
        return false;
    }
}

bool checkNumber(sExp *sExpr) {
    if (sExpr != NULL) {
        if (sExpr->type == LONG || sExpr->type == DOUBLE) {
            return true;
        } else {
            return false;
        }
    } else {
        return false;
    }
}

bool checkString(sExp *sExpr) {
    if (sExpr != NULL) {
        if (sExpr->type == STRING) {
            return true;
        } else {
            return false;
        }
    } else {
        return false;
    }
}

bool checkList(sExp *sExpr) {
    if (checkNil(sExpr)) {
        return true;
    } else if (sExpr->type == CONS) {
        return true;
    } else {
        return false;
    }
} 

////////////////////////////////////////////////////////////////////////////////////////////////////
//Creating sExps
// Initializes an empty S-exp
sExp *createEmptysExp(enum sExpType type) {
    sExp *a = malloc(sizeof(sExp));
    if (a == NULL) {
        printf("Creation of empty S-Expression : malloc failed");
        exit(1);
    }
    a->type = type;
    a->longInt = 0;
    a->doubleInt = 0.0;
    a->symbol = NULL;
    a->string = NULL;
    a->car = NULL;
    a->cdr = NULL;
    return a;
}

// Create a new cons cell
sExp *cons(sExp* carVal, sExp* cdrVal) {
    sExp* cell =  malloc(sizeof(sExp));
    //Sets the values
    cell->type = CONS;
    cell->car = carVal;

    // If cdrVal is NULL, treat as NIL
    if (cdrVal == NULL) {
        cell->cdr = createEmptysExp(NIL);
    } else {
        cell->cdr = cdrVal;
    }

    return cell;
}

// Accessors
sExp *car(sExp* list) {
    if (list != NULL && list->type == CONS) {
        return list->car;
    } else {
        return NULL;
    }
}

sExp *cdr(sExp* list) {
    if (list != NULL && list->type == CONS) {
        return list->cdr;
    } else {
        return NULL;
    }
}

sExp *cadr(sExp* list) {
    return car(cdr(list));
}

sExp *cdar(sExp* list) {
    return cdr(car(list));
}

sExp *caddr(sExp* list) {
    return car(cdr(cdr(list)));
}

sExp *cadddr(sExp* list) {
    return car(cdr(cdr(cdr(list))));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
//Parsing:
// Parse atoms: LONG, DOUBLE, STRING, SYMBOL
sExp *atomSorter(char **line) {
    //Skips the whitespace in front of the atom
    while (**line != '\0' && isspace(**line)) {
        (*line)++;
    }

    char *start = *line; // Start of the input of the sExp
    sExp *atom = NULL;
 
    // STRING
    if (*start == '"') {
        atom = createEmptysExp(STRING); //Creates a new sExp with the type of a string
        (*line)++;  //skip opening quote
        start = *line; //start is now at the first char of the string

        // Finds the closing quote
        char *endQuote = strchr(start, '"');
        //If no endQoute is found, frees the atom
        if (endQuote == NULL) {
            printf("Error: No closing quote\n");
            free(atom);
            return NULL;
        }

        size_t len = endQuote - start;
        char *copy = malloc(len + 1); //Added 1 because the opening quote was skipped
        //If the malloc failed
        if (copy == NULL) {
            printf("malloc failed\n");
            free(atom);
            exit(1);
        }

        memcpy(copy, start, len); //copies the string content over to a temp copy
        copy[len] = '\0'; //null terminates the copy
        atom->string = copy; //sets the atom string value to that copy

        *line = endQuote + 1;
        return atom;
    }

    char *end;

     // LONG
    long lVal = strtol(start, &end, 10); //Converts the string to a LONG
    if (end != start && (*end == '\0' || isspace(*end) || *end == ')' || *end == '(')) {
        atom = createEmptysExp(LONG); //atom->type = LONG;
        atom->longInt = lVal;
        *line = end; //moves the pointer past the number
        return atom;
    }

     // DOUBLE
    double dVal = strtod(start, &end); //Converts the string to a DOUBLE
    if (end != start && (*end == '\0' || isspace(*end) || *end == ')' || *end == '(')) {
        atom = createEmptysExp(DOUBLE); //atom->type = DOUBLE;
        atom->doubleInt = dVal;
        *line = end; //moves the pointer past the number
        return atom;
    }

    // SYMBOL
    *line = start; // reset to beginning
    atom = createEmptysExp(SYMBOL); //atom->type = SYMBOL;

     // Move the pointer until whitespace or parentheses
    while (**line != '\0' && !isspace(**line) && **line != '(' && **line != ')') {
        (*line)++;
    }

    //Same logic for string here
    size_t symbolLen = *line - start;
    char *symbolCopy = malloc(symbolLen + 1);
    if (symbolCopy == NULL) {
        printf("malloc failed\n");
        free(atom);
        exit(1);
    }
    memcpy(symbolCopy, start, symbolLen);
    symbolCopy[symbolLen] = '\0';
    atom->symbol = symbolCopy;

    return atom;
}

// Parse S-expressions recursively
sExp *sExpSorter(char **line) {
    while (isspace(**line)) {
        (*line)++;
    }

    if (**line == '\'') { //checks if there is a quote -> '
        (*line)++; //skip the '
        sExp *quotedExpr = sExpSorter(line); // parse the following expression
        return cons(createSymbol("quote"), cons(quotedExpr, createEmptysExp(NIL))); //adds the quote and cons it
    }

    //If it reads in an open parathesis it will identify the sExp as a list and parse it
    //If there isn't one then it will identify it as an atom instead and parse it
    if (**line == '(') {
        return parseList(line);
    } else {
        return atomSorter(line);
    }
}

// Parse lists
sExp *parseList(char **line) {
    (*line)++; // skip '('
    //Skips whitespace
    while (isspace(**line)) {
        (*line)++;
    }

    if (**line == ')') { // empty list
        (*line)++;
        return createEmptysExp(NIL); // represents nil ()
    }

    sExp *head = NULL; //Head of list
    sExp *tail = NULL; //Rest of list, helps link the list

    //Loops through the list
    while (**line != '\0') {
        //Skips whitespace before each element in list
        while (isspace(**line)) {
            (*line)++;
        }

        //If it reaches the end of the list
        if (**line == ')') {
            (*line)++;
            break;
        }

        // dotted pair handling
        if (**line == '.') {
            (*line)++; //moves past the dot
            while (isspace(**line)) {
                (*line)++;
            }
    
            sExp *dotCdr = sExpSorter(line);//parse after the dot
            if (tail != NULL) {
                tail->cdr = dotCdr; //links what was found to the previous cdr
            } else {
                tail->cdr = createEmptysExp(NIL);
            }

            while (isspace(**line)) {
                (*line)++;
            }

            if (**line == ')') {
                (*line)++;
            } else {
                printf("Error: expected ')' after dotted pair\n");
            }
            break;
        }

        //For regular elements
        sExp *element = sExpSorter(line);
        sExp *cell = cons(element, createEmptysExp(NIL)); // wrap element in a cons cell

        //Links the new cell
        if (tail != NULL) {
            tail->cdr = cell; // link the new cell
            tail = cell;      // move tail
        } else {
            head = cell; // first cell becomes head
            tail = cell; // tail points to first cell
        }
    }

    if (head == NULL) {
        head = createEmptysExp(NIL);
    }

    return head;
}
////////////////////////////////////////////////////////////////////////////////////////////////////
//Printing
// Print S-exps based off of what type it is
void printSExp(sExp *sExpr) {
    if (sExpr == NULL) {
        printf("()"); // If S-exp is NULL print out nil ie: ()
        return;
    }

    //Switch statement to print out the S-exp based off of the enum type
    switch (sExpr->type) {
        case LONG: {
            printf("%ld", sExpr->longInt);
            break;
        }
        case DOUBLE: {
            printf("%0.2lf", sExpr->doubleInt);
            break;
        }
        case SYMBOL: {
            printf("%s", sExpr->symbol);
            break;
        }
        case STRING: {
            printf("\"%s\"", sExpr->string);
            break;
        }
        case NIL: {//Prints out the list
            //If nil prints out nil, doesn't add extra parathesises
            if (checkNil(sExpr)) {
                printf("()");
                return;
            }
        }
        case CONS: { 
            printf("(");
            sExp *current = sExpr; //Pointer used to travel through the list
            int first = 1; //Used for spaces

            //Travels the list
            while (current != NULL) {
                //Prints a space between every element except the first element
                if (first != 1) { 
                    printf(" "); 
                }
                first = 0;

                //Recursively prints the car elements of the list
                printSExp(car(current));

                //If the rest of the list is NULL, breaks out of the while
                sExp *next = cdr(current);
                if (checkNil(next)) { 
                    break; 
                }
                else if (next->type != CONS) { // If the cdr is not a cons cell or proper list
                    printf(" . "); // prints a dotted pair
                    printSExp(next);
                    break;
                } else { 
                    current = next; //Move to next cell
                }
            }

            printf(")"); //Closes the list
            break;
        }
        default: { //For any unknown types
            printf("()/nil\n"); 
            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////////////////////////
// Frees S-exps
void freeSExp(sExp *sExpr) {
  if (sExpr == NULL) {
        return;
    }

    switch (sExpr->type) {
        case SYMBOL:
            if (sExpr->symbol != NULL) {
                free(sExpr->symbol);
            }
            break;
        case STRING:
            if (sExpr->string != NULL) {
                free(sExpr->string);
            }
            break;
        case CONS:
            freeSExp(car(sExpr)); // free the car
            freeSExp(cdr(sExpr)); // free the cdr
            break;
        default:
            break;
    }

    free(sExpr); //frees the node
}

//Function that tracks the parentheses of an S-Expression so that repl can read in one if it spans multiple lines
int parenthesesCounter(const char *sExp) {
    int bal = 0;
    while (*sExp) {
        if (*sExp == '('){
            bal++; //increases the count
        }
        else if (*sExp == ')'){
            bal--; //decreases the count
        }
        sExp++; //Moves to the next character in the S-Expression
    }
    return bal; // 0 = balanced, positive = missing a closing parenthesis, negative = extra parentheses
}

// Remove comments from a line
char *stripComments(char *line) {
    char *semi = strchr(line, ';');
    if (semi) {
        *semi = '\0'; // replaces the ; with a null terminator to prevent it from reading it in
    }
    // remove trailing newline
    line[strcspn(line, "\n")] = '\0';
    return line;
}


void repl(FILE *input, sExp **rho) {
    char line[1024]; //buffer to read in lines at a time
    char buffer[4096]; //store read in lines until an S-Expression with balanced parentheses has been fully read in
    buffer[0] = '\0'; //intializes the buffer as empty

    while (true) {
            // Prints a prompt only at the start of a new expression
            if (buffer[0] == '\0') {
                printf("> ");
                fflush(stdout); //makes sure the prompt shows immediately
            }

        while (fgets(line, sizeof(line), input)) {//Reads in lines until an sExp has been read in 
            char *stripped = stripComments(line); //Prevents comments from being read in
            while (isspace(*stripped)){
                stripped++; //skips the leading whitespace
            }
            if (*stripped == '\0'){
                continue; //Skips the loop if the line is just a comment or whitespace
            }

            if (strlen(buffer) + strlen(stripped) + 2 >= sizeof(buffer)) {
                fprintf(stderr, "Error: input too long, buffer overflow prevented.\n");
                buffer[0] = '\0';
                break;
            }

            strcat(buffer, stripped); //Copies the read in line into the buffer
            strcat(buffer, " "); // ensures read in lines in buffer don't merge if an sExp spans multiple lines
        

            int bal = parenthesesCounter(buffer);

            if(bal > 0){//If there aren't enough parentheses it will keep looping until enough are entered
                continue; 
            } else{
                break;
            }
        }

        //File read in & there is no more data in buffer
        if (buffer[0] == '\0'){
            break;
        }

        // Parse multiple S-expressions in the buffer
        while (buffer[0] != '\0') {
            char *itr = buffer;

            // Skip leading whitespace and extra closing parentheses
            while (*itr && (isspace(*itr) || *itr == ')')){
                itr++;
            }

            if (*itr == '\0') {
                buffer[0] = '\0'; // only extra ')' or spaces left
                break;
            }

            sExp *sExpr = sExpSorter(&itr); // parse the input into an s-expression
            if (sExpr == NULL) {
                printf("Error: could not parse expression\n");
                buffer[0] = '\0'; // clear buffer on parse error
                break;
            }

            sExp *result = eval(sExpr, rho);
            
            printSExp(result);
            printf("\n\n");

            freeSExp(sExpr);
            freeSExp(result);

            // Move leftover input to start of buffer manually
            if (*itr != '\0') {
                // skip leading whitespace
                while (isspace(*itr)){
                    itr++;
                }
                int i = 0;
                while (itr[i] != '\0') {
                    buffer[i] = itr[i];
                    i++;
                }
                buffer[i] = '\0'; // null terminate
            } else {
                buffer[0] = '\0';
            }
        }
    }
}