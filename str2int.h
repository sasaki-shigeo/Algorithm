extern long str2long(char *);
static inline int str2int(char *str) {
    return (int)(str2long(str));
}
