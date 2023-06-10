#include <stdlib.h>
#include <stdio.h>
#include <float.h>

int main() {
    puts("float:");
    printf("significand bits = %d\n", FLT_MANT_DIG);
    printf("min exp part = %d\n", FLT_MIN_EXP);
    printf("max exp part = %d\n", FLT_MAX_EXP);
    printf("machine epsilon = %G (%a) = 0x0.000002P0\n", FLT_EPSILON, FLT_EPSILON);
    printf("max = %G (%a)\n", FLT_MAX, FLT_MAX);
    printf("min positive normal = %G (%a)\n", FLT_MIN, FLT_MIN);
    float flt_min_denormal = (float)FLT_EPSILON * (float)FLT_MIN;
    printf("min positive denormal = %G (%a)\n", flt_min_denormal, flt_min_denormal);
    puts("\ndouble:");
    printf("significand bits = %d\n", DBL_MANT_DIG);
    printf("min exp part = %d\n", DBL_MIN_EXP);
    printf("max exp part = %d\n", DBL_MAX_EXP);
    printf("machine epsilon = %G (%a)\n", DBL_EPSILON, DBL_EPSILON);
    printf("min positive normal = %G (%a)\n", DBL_MIN, DBL_MIN);
    double dbl_min_denormal = DBL_EPSILON * DBL_MIN;
    printf("min positive denormal = %G (%a)\n", dbl_min_denormal, dbl_min_denormal);
    puts("\nlong double:");
    printf("significand bits = %d\n", LDBL_MANT_DIG);
    printf("min exp part = %d\n", LDBL_MIN_EXP);
    printf("max exp part = %d\n", LDBL_MAX_EXP);
    printf("machine epsilon = %LG (%La)\n", LDBL_EPSILON, LDBL_EPSILON);
    printf("min positive normal = %LG (%La)\n", LDBL_MIN, LDBL_MIN);
    long double ldbl_min_denormal = LDBL_EPSILON * LDBL_MIN;
    printf("min positive denormal = %LG (%La)\n", ldbl_min_denormal, ldbl_min_denormal);

    exit(0);
}
