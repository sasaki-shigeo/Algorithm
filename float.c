#include <stdlib.h>
#include <stdio.h>
#include <float.h>

int main() {
    puts("float\n");
    printf("significand bits = %d\n", FLT_MANT_DIG);
    printf("min exp part = %d\n", FLT_MIN_EXP);
    printf("max exp part = %d\n", FLT_MAX_EXP);
    printf("max = %g (%a)\n", FLT_MAX, FLT_MAX);
    printf("min positive normal = %g (%a)\n", FLT_MIN, FLT_MIN);
    printf("machine epsilon = %g (%a) = 0x0.000002P0\n", FLT_EPSILON, FLT_EPSILON);
    printf("min positive denormal = %g\n", (float)FLT_MIN * (float)FLT_EPSILON);
    puts("\ndouble\n");
    printf("significand bits = %d\n", DBL_MANT_DIG);
    printf("min exp part = %d\n", DBL_MIN_EXP);
    printf("max exp part = %d\n", DBL_MAX_EXP);
    printf("min positive normal = %g\n", DBL_MIN);
    printf("machine epsilon = %g\n", DBL_EPSILON);
    puts("\nlong double\n");
    printf("significand bits = %d\n", LDBL_MANT_DIG);
    printf("min exp part = %d\n", LDBL_MIN_EXP);
    printf("max exp part = %d\n", LDBL_MAX_EXP);
    printf("min positive normal = %Lg\n", LDBL_MIN);
    printf("machine epsilon = %Lg\n", LDBL_EPSILON);

    exit(0);
}
