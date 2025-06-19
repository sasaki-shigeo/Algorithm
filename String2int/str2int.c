#include <stdlib.h>
#include <stdio.h>
#include "str2int.h"

long str2long(char *str) {
    long result = 0;
    int c;

START:
    c = *(str++);
    if (c == '\0') {
        return 0;
    }
    else if (c == '0') {
        goto ZERO;
    }
    else if ('0' <= c && c <= '9') {
        result = c - '0';
        goto DEC;
    }
    else {
        return result;
    }

ZERO:
    c = *(str++);
    if (c == 'x' || c == 'X') {
        goto X;
    }
    else if ('0' <= c && c <= '7') {
        result = c - '0';
        goto OCT;
    }
    else {
        return result;
    }

DEC:
    c = *(str++);
    if ('0' <= c && c <= '9') {
        result *= 10;
        result += c - '0';
        goto DEC;
    }
    else {
        return result;
    }

OCT:
    c = *(str++);
    if ('0' <= c && c <= '7') {
        result *= 8;
        result += c - '0';
        goto OCT;
    }
    else {
        return result;
    }

X:
    c = *(str++);
    if ('0' <= c && c <= '9') {
        result = c - '0';
        goto HEX;
    }
    else if ('A' <= c && c <= 'F') {
        result = c - 'A' + 10;
        goto HEX;
    }
    else if ('a' <= c && c <= 'f') {
        result = c - 'a' + 10;
        goto HEX;
    }
    else {
        return -1;
    }

HEX:
    c = *(str++);
    if ('0' <= c && c <= '9') {
        result *= 16;
        result += c - '0';
        goto HEX;
    }
    else if ('A' <= c && c <= 'F') {
        result *= 16;
        result += c - 'A' + 10;
        goto HEX;
    }
    else if ('a' <= c && c <= 'f') {
        result *= 16;
        result += c - 'a' + 10;
        goto HEX;
    }
    else {
        return result;
    }

ERROR:
    fprintf(stderr, "internal error\n");
    exit(1);

ACCEPT:            /* use return result; instead goto ACCCEPT */
    return result;
}

// data for testing
static char *literals[] = {
    "0", "123", "077", "0xff", "0XFF",
    NULL
};

// test function
int main(void) {
    for (char **p = literals; *p != NULL; p++) {
        printf("%s = %d\n", *p, str2int(*p));
    }
    exit(0);
}
