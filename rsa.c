#include <stdlib.h>
#include <stdio.h>

int gcd(int m, int n) {
    int r = m % n;
    if (r == 0) {
	return n;
    }
    else {
	return gcd(n, r);
    }
}

long lgcd(long m, long n) {
    long r = m % n;
    if (r == 0) {
	return n;
    }
    else {
	return lgcd(n, r);
    }
}

int lcm(int m, int n) {
    return m / gcd(m, n) * n;
}

int llcm(long m, long n) {
    return m / lgcd(m, n) * n;
}

const int p = 4327;
const int q = 60013;
const int n = p * q;
const int L = 43268652;
const int e = 65537;
const int d = 42152885;

int slow_powmod(int x, unsigned int k, int n) {
    long y = 1L;
    for (int i = 0; i < k; i++) {
	y *= x;
	y %= n;
    }
    return (int)y;
}

int fast_powmod(int x, unsigned int k, int n) {
    if (k == 0) {
	return 1L;
    }
    else if (k % 2 == 0) {
	long y = fast_powmod(x, k/2, n);
	return (int)(y * y % n);
    }
    else {
	long y = fast_powmod(x, k - 1, n);
	return (int)(x * y % n);
    }
}

int fast_fast_powmod(int x, unsigned int k, int n) {
    const unsigned int MSB = 1 << (8 * sizeof(int) - 1);
    long y = 1L;
    for (unsigned int bit = MSB; bit != 0; bit >>= 1) {
	y = (y * y) % n;
	if (bit & k) {
	    y = y * x % n;
	}
    }
    return (int)y;
}

int powmod(int x, int k, int n) {
    return fast_powmod(x, k, n);
}

int encrypt(int m) {
    return powmod(m, e, n);
}

int decrypt(int c) {
    return powmod(c, d, n);
}

int main() {
    sranddev();
    int m = rand() % n;
    int c = encrypt(m);
    int m2 = decrypt(c);
    printf("original text = %d, encrypted text = %d, decrypted text = %d\n", m, c, m2);
    
    exit(0);
}
