// Adapt the usual "naive" way to compute partition function using
// memoization

#include "util.h"

ull primes[1000];
int memo[1000][1000];

int numprimepartitions(int n, int maxidx) {
  if (n < 0) return 0;
  if (n == 0) return 1;

  ull curprime;
  ull res = 0;
  for (int i = 0; i < 1000; i++) {
    curprime = primes[i];

    if (curprime > (ull)n || curprime > primes[maxidx])
      break;

    int prevres = memo[n-curprime][i];
    if (prevres == -1)
      prevres = numprimepartitions(n-curprime, i);
    res += prevres;
  }

  memo[n][maxidx] = res;
  return res;
}

int main() {
  firstnprimes(primes, 1000);
  memset(memo, -1, 1000*1000);

  for (int i = 0; i < 100; i++)
    printf("f(%d) = %d\n", i, numprimepartitions(i, 999));
}