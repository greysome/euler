// Just brute force over triples of primes lol
// We only need to iterate over primes p < sqrt(50 million) = 7072

#include "util.h"

#define N 50000000
#define SQRT 7072

ull primes[1000];
int expressible[N] = {0};

int main() {
  int numprimes = primestill(primes, SQRT);
  for (int i = 0; i < numprimes; i++) {
    for (int j = 0; j < numprimes; j++) {
      for (int k = 0; k < numprimes; k++) {
	ull p = primes[i];
	ull q = primes[j];
	ull r = primes[k];
	ull x = p*p + q*q*q + r*r*r*r;
	if (x < 50000000)
	  expressible[x] = 1;
      }
    }
  }

  int count = 0;
  for (int i = 0; i < N; i++)
    if (expressible[i]) count++;
  printf("%d\n", count);
}