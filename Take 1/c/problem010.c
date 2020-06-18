#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdint.h>
#include <math.h>

#define SIZE 2000000

int main(int argc, char** argv) {
    
    uint8_t sieve[SIZE];
    uint32_t i, j;

    for (i = 0; i < SIZE; i++) {
        sieve[i] = 1;
    }
    
    uint32_t upper_bound = (uint32_t) ceil(sqrt(SIZE));
    for (i = 2; i < upper_bound; i++) {
        if (sieve[i] == 1) {
            for (j = i+i; j < SIZE; j += i) {
                sieve[j] = 0;
            }
        }
    }

    uint64_t sum = 0;
    for (i = 2; i < SIZE; i++) {
        if (sieve[i] == 1) {
            sum += i;
        }
    }
    printf("%llu\n", sum);
}





