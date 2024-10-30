#include "util.h"

// Sum of digit factorials
int chain_next(int x) {
  int xdigits[16];
  int n = digits(x, xdigits);
  int sum = 0;
  for (int i = 0; i < n; i++)
    sum += fact(xdigits[i]);
  return sum;
}

int main() {
  int num_chains = 0;
  for (int n = 0; n < 1000000; n++) {
    int nonrepeats[60];
    int next = n;

    for (int k = 0; k <= 60; k++) {
      bool b = contains(nonrepeats, next, k);

      if (b && k < 60) break;
      if (b && k == 60) num_chains++;

      nonrepeats[k] = next;
      next = chain_next(next);
    }
  }

  printf("%d\n", num_chains);
} 