#include "util.h"

int main() {
  float min = INFINITY;
  int min_n;

  for (int n = 2; n < 10000000; n++) {
    ull p = phi(n);
    if (samedigits(n, p)) {
      printf("%d, min=(%d,%f)\n", n, min_n, min);
      if (((float)n)/p < min) {
	min = ((float)n) / p;
	min_n = n;
      }
    }
  }
}