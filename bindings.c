#include <stdio.h>

extern double putchard(double X) {
    putchar((char)X);
    return 0;
}

extern double printd(double X) {
    printf("%f\n", X);
    return 0;
}