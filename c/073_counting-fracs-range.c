#include "util.h"

int main() {
  ull count = 0;
  for (int a = 1; a < 12000; a++) {
    for (int b = 2*a+1; b < 3*a && b <= 12000; b++) {
      if (gcd(a,b) == 1) count++;
    }
  }
  printf("%lld\n", count);
}