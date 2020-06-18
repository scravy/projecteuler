#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define SIZE 1000000
#define UPTO 1000000

int main(int argc, char** argv)
{
    uint64_t seq[SIZE];

    uint32_t i;
    for (i = 0; i < SIZE; i++) {
        seq[i] = 0;
    }

    uint64_t n;
    uint64_t c = 1;
    uint64_t max = 0;
    uint64_t max_n = 0;
    for (i = 2; i < UPTO; i++) {
        n = i;
        while (n > 1) {
            if (n % 2 == 0) {
                n /= 2;
            } else {
                n = 3*n + 1;
            }
            if (n < SIZE) {
                if (seq[n] != 0) {
                    c += seq[n];
                    break;
                }
            }
            c++;
        }
        seq[i] = c;
        if (c > max) {
            max = c;
            max_n = i;
        }
        c = 1;
    }
    printf("%llu\n", max_n);

    return 0;
}


