#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "str2int.h"

long str2long(char *str) {
    enum { ST_START, ST_ZERO, ST_DEC, ST_OCT, ST_X, ST_HEX } state = ST_START;
    long result = 0;

    for (int c = *(str++); c != '\0'; c = *(str++)) {
        switch (state) {
            case ST_START:
                if (c == '0') {
                    state = ST_ZERO;
                    continue;
                }
                else if (isdigit(c)) {
                    state = ST_DEC;
                    result = c - '0';
                    continue;
                }
                else {
                    return result;
                }
            case ST_ZERO:
                if (c == 'x' || c == 'X') {
                    state = ST_X;
                    continue;
                }
                else if ('0' <= c && c <= '7') {
                    state = ST_OCT;
                    result = c - '0';
                    continue;
                }
                else {
                    return result;
                }
            case ST_DEC:
                if (isdigit(c)) {
                    result *= 10;
                    result += c - '0';
                    continue;
                }
                else {
                    return result;
                }
            case ST_OCT:
                if ('0' <= c && c <= '7') {
                    result *= 8;
                    result += c - '0';
                    continue;
                }
                else {
                    return result;
                }
            case ST_X:
                if (isdigit(c)) {
                    result = c - '0';
                    continue;
                }
                else if ('A' <= c && c <= 'F') {
                    result = c - 'A' + 10;
                    continue;
                }
                else if ('a' <= c && c <= 'f') {
                    result = c - 'a' + 10;
                    continue;
                }
                else {
                    return -1;
                }
            case ST_HEX:
                if (isdigit(c)) {
                    result *= 16;
                    result += c - '0';
                    continue;
                }
                else if ('A' <= c && c <= 'F') {
                    result *= 16;
                    result += c - 'A' + 10;
                    continue;
                }
                else if ('a' <= c && c <= 'f') {
                    result *= 16;
                    result += c - 'a' + 10;
                    continue;
                }
                else {
                    return result;
                }
            default:
                fprintf(stderr, "internal error\n");
                exit(1);
        }
    }
    return result;
}


static char *literals[] = {
    "0", "123", "077", "0xff", "0XFF",
    NULL
};

int main() {
    for (char **p = literals; *p != NULL; p++) {
        printf("%s = %d\n", *p, str2int(*p));
    }
    exit(0);
}
