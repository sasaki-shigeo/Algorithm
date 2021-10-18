#include <string.h>
#include <stdio.h>

#define NUL '\0'

int checksum10(char *digits) {
    int sum = 0;
    for (int i = 0; i < 9; i++) {
	sum += (10 - i) * (digits[i] - '0');
    }
    if (digits[9] == NUL) {
	return sum % 11;
    }
    else if (digits[9] == 'X') {
	sum += 10;
    }
    else {
	sum += (digits[9] - '0');
    }
    return sum % 11;
}

int checksum(char *digits) {
    int sum = 0;
    for (int i = 0; i < 12; i++) {
	if ((i & 1) == 0) {
	    sum += (digits[i] - '0');
	}
	else {
	    sum += 3 * (digits[i] - '0');
	}
    }
    if (digits[12] != NUL) {
	sum += (digits[12] - '0');
    }

    return sum % 10;
}

int main(int argc, char* argv[]) {
    int sum = 0;
    if (argc <= 1) {
	fprintf(stderr, "Usage: %s <code>\n", argv[0]);
	return 1;
    }
    // printf("len of %s is %ld\n", argv[1], strnlen(argv[1], 14));
    // exit(0);
    for (int i = 1; i < argc; i++) {
	switch (strnlen(argv[i], 14)) {
	    case 9:
		sum = checksum10(argv[i]);
		if (sum == 0) {
		    printf("X\n");
		}
		else {
		    printf("%d\n", 11 - sum);
		}
		continue;
	    case 10:
		sum = checksum10(argv[i]);
		if (sum == 0) {
		    printf("OK.\n");
		}
		else {
		    printf("not correct.\n");
		}
		continue;
	    case 12:
		sum = checksum(argv[i]);
		printf("%d\n", (10 - sum) % 10);
		continue;
	    case 13:
		sum = checksum(argv[i]);
		if (sum == 0) {
		    printf("OK.\n");
		}
		else {
		    printf("not correct.\n");
		}
		continue;
	    case 14:
		fprintf(stderr, "bad argument %14s...\n", argv[i]);
		return 2;
	    default:
		fprintf(stderr, "bad argument %s\n", argv[i]);
		return 3;
	}
    }
    return 0;
}
