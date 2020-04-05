#include <stdio.h>

int print_test() {
    printf("print_test\n");
    return 0;
}

int print_integer(int x) {
    printf("print_integer: %d\n", x);
    return 0;
}

int print_add(int a, int b) {
    int x = a + b;
    printf("print_add: %d\n", x);
    return x;
}
