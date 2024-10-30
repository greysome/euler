// Adapted solution for 76, but taking mod 1000000 all the way

#include "util.h"

#define N 100000

int cache[N];

int pmod(int n) {
  if (n < 0) return 0;
  if (cache[n] != -1) return cache[n];

  int result = 0;

  for (int i = 1; i < N; i++) {
    int k = (3*i*i-i)/2;
    if (k > n) break;
    int sign = i % 2 == 0 ? -1 : 1;
    result = (result + sign * pmod(n - k)) % 1000000;
  }

  for (int i = -1; i > -N; i--) {
    int k = (3*i*i-i)/2;
    if (k > n) break;
    int sign = i % 2 == 0 ? -1 : 1;
    result = (result + sign * pmod(n - k)) % 1000000;
  }

  cache[n] = result % 1000000;
  return result % 1000000;
}

int main() {
  for (int i = 2; i < N; i++)
    cache[i] = -1;
  cache[0] = 1;
  cache[1] = 1;

  for (int i = 0; i < N; i++) {
    int pmodi = pmod(i);
    printf("%d %d\n", i, pmodi);
    if (pmodi == 0)
      return 0;
  }
}