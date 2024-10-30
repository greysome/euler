// Evaluating p(100) via the recurrence
// p(n) = p(n-1) + p(n-2) - p(n-5) - p(n-7) + p(n-12) + p(n-15) - ...
// (See Euler's pentagonal number theorem)

#include "util.h"

int offsets[16] = {1,2,5,7,12,15,22,26,35,40,51,57,70,77,92,100};
ll cache[101];

ll p(int n) {
  if (n < 0) return 0;
  if (cache[n] != -1) return cache[n];

  ll result = 0;
  for (int i = 0; i < 16; i++) {
    int sign = (i/2) % 2 == 0 ? 1 : -1;
    result += sign * p(n - offsets[i]);
  }
  cache[n] = result;
  return result;
}

int main() {
  for (int i = 0; i < 101; i++)
    cache[i] = -1;
  cache[0] = 1;
  cache[1] = 1;

  printf("%lld\n", p(100)-1);
}