#include <stdlib.h>
#include <stdio.h>
#include <strings.h>
#include <stdint.h>

FILE *fp;
int strict_mode = 1;

static char buf[BUFSIZ];
static char base64chars[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static int base64index[] = {
    -1, -1, -1, -1, -1, -1, -1, -1, // 0x00
    -1, -1, -1, -1, -1, -1, -1, -1, // 0x08
    -1, -1, -1, -1, -1, -1, -1, -1, // 0x10
    -1, -1, -1, -1, -1, -1, -1, -1, // 0x18
    -1, -1, -1, -1, -1, -1, -1, -1, // 0x20
    -1, -1, -1, 62, -1, -1, -1, 63, // 0x28
    52, 53, 54, 55, 56, 57, 58, 59, // 0x30
    60, 61, -1, -1, -1, -1, -1, -1, // 0x38
    -1,  0,  1,  2,  3,  4,  5,  6, // 0x40
     7,  8,  9, 10, 11, 12, 13, 14, // 0x48
    15, 16, 17, 18, 19, 20, 21, 22, // 0x50
    23, 24, 25, -1, -1, -1, -1, -1, // 0x58
    -1, 26, 27, 28, 29, 30, 31, 32, // 0x60
    33, 34, 35, 36, 37, 38, 39, 40, // 0x68
    41, 42, 43, 44, 45, 46, 47, 48, // 0x70
    49, 50, 51, -1, -1, -1, -1, -1  // 0x78
};

void make_index() {
    for (int i = 0; i < 128; i++) {
        base64index[i] = index(base64chars, i) - base64chars;
    }
}

size_t base64encode(char *dest, void *src, size_t srcsize) {
    size_t limit = (srcsize <= 3 * 16 ? srcsize : 3 * 16);

    for (size_t n = 0; n < limit; n += 3) {
        uint8_t *p = src + n;
        switch (limit - n) {
            case 0:
                *dest = '\0';   // '\0' is the string terminator
                return n;
            case 1:
                *(dest++) = base64chars[p[0] >> 2];
                *(dest++) = base64chars[(p[0] & 0x03) << 4];
                *(dest++) = '=';
                *dest     = '\0';
                return n;
            case 2:
                *(dest++) = base64chars[p[0] >> 2];
                *(dest++) = base64chars[((p[0] & 0x03) << 4) |
                                        ((p[1] & 0xf0) >> 4)];
                *(dest++) = base64chars[(p[1] & 0x0f) << 2];
                *(dest++) = '=';
                *dest     = '\0';
                return n;
            default:
                *(dest++) = base64chars[p[0] >> 2];
                *(dest++) = base64chars[((p[0] & 0x03) << 4) |
                                       ((p[1] & 0xf0) >> 4)];
                *(dest++) = base64chars[((p[1] & 0x0f) << 2) |
                                       ((p[2] & 0xc0) >> 6)];
                *(dest++) = base64chars[p[2] & 0x3f];
        }
    }
}

size_t base64decode(char *src, void *dest, size_t destlimit) {
    uint8_t *buf = dest;

    for (size_t sz = 0; sz < destlimit; ) {
        int ix0 = base64index[*(src++)];
        if (ix0 < 0) {  // maybe *src == '\0' or '\n'
            return sz;
        }

        int ix1 = base64index[*(src++)];
        if (ix1 < 0) {   // illeagal
            return sz;
        }

        if (*src == '=') {
            if ((ix1 & 0x0f) != 0) {
                return sz;
            }
            else {
                buf[sz++] = (uint8_t)((ix0 << 2) | (ix1 >> 4));
                return sz;
            }
        }
        
        int ix2 = base64index[*(src++)];
        if (ix2 < 0) {    // illegal
            return sz;
        }

        if (*src == '=') {
            if ((ix & 0x03) != 0) {
                return sz;
            }
            else {
                buf[sz++] = (uint8_t)((ix0 << 2) | (ix1 >> 4));
                buf[sz++] = (uint8_t)((ix1 << 4) | (ix2 >> 2));
                return sz;
            }
        }

        int ix3 = base64index[*(src++)];
        if (ix3 < 0) {    // illegal
            return sz;
        }
        buf[sz++] = (uint8_t)((ix0 << 2) | (ix1 >> 4));
        buf[sz++] = (uint8_t)((ix1 << 4) | (ix2 >> 2));
        buf[sz++] = (uint8_t)((ix2 << 6) | ix3);
    }
    return sz;
}

int main(int argc, char *argv[]) {
    char buf[65];
    union {
        int64_t longval;
        uint8_t bytearray[8];
    } eight_bytes;
    
    for (eight_bytes.longval = 0; eight_bytes.longval < 1024; eight_bytes.longval++) {
        /* printf("%02x %02x %02x %02x %02x %02x %02x %02x\n",
               eight_bytes.bytearray[0],
               eight_bytes.bytearray[1],
               eight_bytes.bytearray[2],
               eight_bytes.bytearray[3],
               eight_bytes.bytearray[4],
               eight_bytes.bytearray[5],
               eight_bytes.bytearray[6],
               eight_bytes.bytearray[7]); */
        int n = base64encode(buf, eight_bytes.bytearray, 8);
        puts(buf);
    }

    // int n = base64encode(buf, "\0\0\0", strlen(bytes));
    // puts(buf);
}
