// This code is HORRIBLY inefficient but it does the job.

#include "util.h"

// The most naive implementation, because I was lazy
// I plan to implement a more powerful factorization algorithm in the future
int smallest_factor(int n) {
  for (int i = 2; i <= (int)introot(n,2); i++)
    if (n%i == 0)
      return i;
  return n;
}

// Computes _pk = p^k, where k is the multiplicity of p in n,
// as well as _phi = phi(p^k).
void phi_pk(int n, int p, int *_pk, int *_phi) {
  int pk = 1, phi = 1;
  while (n % (pk*p) == 0) {
    phi *= pk == 1 ? p-1 : p;
    pk *= p;
  }
  *_pk = pk;
  *_phi = phi;
}

float max = 0;
int argmax;

int main() {
  for (int n = 2; n < 1000000; n++) {
    printf("%d\n", n);

    // Compute phi
    int phi = 1;
    int m = n;
    int p, pk, a;
    while ((p = smallest_factor(m)) > 1) {
      phi_pk(m, p, &pk, &a);
      phi *= a; m /= pk;
    }

    // Compare phi with current max
    float ratio = ((float) n) / phi;
    if (ratio > max) {
      max = ratio;
      argmax = n;
    }
  }

  printf("%d %f\n", argmax, max);
}